-module(shout).

%% 하나의 창을 열어 > shout:start()
%% 다른 창에서 xmms http://localhost:3000/stream

-export([start/0]).
-import(lists, [map/2, reverse/1]).

-define(CHUNKSIZE, 24576).

start() ->
    spawn(fun() ->
		  start_parallel_server(3000),
		  %% 이제 sleep 상태로 - 그렇지 않으면 리스닝 소켓은 닫힐 것임.
		  lib_misc:sleep(infinity)
	  end).

start_parallel_server(Port) ->
    {ok, Listen} = gen_tcp:listen(Port, [binary, {packet, 0},
					 {reuseaddr, true},
					 {active, true}]),
    PidSongServer = spawn(fun() -> songs() end),
    spawn(fun() -> par_connect(Listen, PidSongServer) end).

par_connect(Listen, PidSongServer) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    spawn(fun() -> par_connect(Listen, PidSongServer) end),
    inet:setopts(Socket, [{packet,0},binary, {nodelay,true},{active, true}]),
    get_request(Socket, PidSongServer, []).

get_request(Socket, PidSongServer, L) ->
    receive
	{tcp, Socket, Bin} ->
	    L1 = L ++ binary_to_list(Bin),
	    %% 헤더가 완결되었는지 split 검사
	    case split(L1, []) of
		more ->
		    %% 헤더가 아직 남아서 데이터가 더 필요.
		    get_request(Socket, PidSongServer, L1);
		{Request, _Rest} ->
		    %% 헤더가 완결됨
		    got_request_from_client(Request, Socket, PidSongServer)
	    end;
	{tcp_closed, Socket} ->
	    void;
	_Any ->
	    %% 건너뜀
	    get_request(Socket, PidSongServer, L)
    end.

split("\r\n\r\n" ++ T, L) -> {reverse(L), T};
split([H|T], L)           -> split(T, [H|L]);
split([], _)              -> more.

got_request_from_client(Request, Socket, PidSongServer) ->
    Cmds = string:tokens(Request, "\r\n" ),
    Cmds1 = map(fun(I) -> string:tokens(I, " " ) end, Cmds),
    is_request_for_stream(Cmds1),
    gen_tcp:send(Socket, [response()]),
    play_songs(Socket, PidSongServer, <<>>).

play_songs(Socket, PidSongServer, SoFar) ->
    Song = rpc(PidSongServer, random_song),
    {File,PrintStr,Header} = unpack_song_descriptor(Song),
    case id3_tag_lengths:file(File) of
	error ->
	    play_songs(Socket, PidSongServer, SoFar);
	{Start, Stop} ->
	    io:format("Playing:~p~n" ,[PrintStr]),
	    {ok, S} = file:open(File, [read,binary,raw]),
	    SoFar1 = send_file(S, {0,Header}, Start, Stop, Socket, SoFar),
	    file:close(S),
	    play_songs(Socket, PidSongServer, SoFar1)
    end.

send_file(S, Header, OffSet, Stop, Socket, SoFar) ->
    %% OffSet = 연주할 첫 번째 바이트
    %% Stop = 연주할 수 있는 마지막 바이트
    Need = ?CHUNKSIZE - size(SoFar),
    Last = OffSet + Need,
    if
	Last >= Stop ->
	    %% 데이터가 충분하지 않으므로 읽을 수 있을 때까지 읽고 반환
	    Max = Stop - OffSet,
	    {ok, Bin} = file:pread(S, OffSet, Max),
	    list_to_binary([SoFar, Bin]);
	true ->
	    {ok, Bin} = file:pread(S, OffSet, Need),
	    write_data(Socket, SoFar, Bin, Header),
	    send_file(S, bump(Header),
		      OffSet + Need, Stop, Socket, <<>>)
    end.

write_data(Socket, B0, B1, Header) ->
    %% 정말로 올바른 크기의 블록을 받았는지 검사
    %% 이 루틴은 우리 프로그램 로직이 올바른지 검사하는 데 매우 유용함
    case size(B0) + size(B1) of
	?CHUNKSIZE ->
	    case gen_tcp:send(Socket, [B0, B1, the_header(Header)]) of
		ok -> true;
		{error, closed} ->
		    %% 연주자가 접속을 종료한 경우 발생
		    exit(playerClosed)
	    end;
	_Other ->
	    %% 블록을 보내지 말 것-오류 보고
	    io:format("Block length Error: B0 = ~p b1=~p~n" ,
		      [size(B0), size(B1)])
    end.

bump({K, H}) -> {K+1, H}.

the_header({K, H}) ->
    case K rem 5 of
	0 -> H;
	_ -><<0>>
    end.

is_request_for_stream(_) -> true.

response() ->
    ["ICY 200 OK\r\n" ,
     "icy-notice1: <BR>This stream requires" ,
     "<a href=\" http://www.winamp.com/\">Winamp</a><BR>\r\n" ,
     "icy-notice2: Erlang Shoutcast server<BR>\r\n" ,
     "icy-name: Erlang mix\r\n" ,
     "icy-genre: Pop Top 40 Dance Rock\r\n" ,
     "icy-url: http://localhost:3000\r\n" ,
     "content-type: audio/mpeg\r\n" ,
     "icy-pub: 1\r\n" ,
     "icy-metaint: " ,integer_to_list(?CHUNKSIZE),"\r\n" ,
     "icy-br: 96\r\n\r\n" ].

songs() ->
    {ok,[SongList]} = file:consult("mp3data" ),
    lib_misc:random_seed(),
    songs_loop(SongList).

songs_loop(SongList) ->
    receive
	{From, random_song} ->
	    I = random:uniform(length(SongList)),
	    Song = lists:nth(I, SongList),
	    From ! {self(), Song},
	    songs_loop(SongList)
    end.

rpc(Pid, Q) ->
    Pid ! {self(), Q},
    receive
	{Pid, Reply} ->
	    Reply
    end.

unpack_song_descriptor({File, {_Tag,Info}}) ->
    PrintStr = list_to_binary(make_header1(Info)),
    L1 = ["StreamTitle='" ,PrintStr,
	  "';StreamUrl='http://localhost:3000';" ],
    %% io:format("L1=~p~n",[L1]),
    Bin = list_to_binary(L1),
    Nblocks = ((size(Bin) - 1) div 16) + 1,
    NPad = Nblocks*16 - size(Bin),
    Extra = lists:duplicate(NPad, 0),
    Header = list_to_binary([Nblocks, Bin, Extra]),
    %% Header는 Shoutcast 헤더임.
    {File, PrintStr, Header}.

make_header1([{track,_}|T]) ->
    make_header1(T);
make_header1([{Tag,X}|T]) ->
    [atom_to_list(Tag),": " ,X," " |make_header1(T)];
make_header1([]) ->
    [].
