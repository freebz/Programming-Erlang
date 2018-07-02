-module(broadcast).
-compile(export_all).
send(IoList) ->
    case inet:ifget("eth0" , [broadaddr]) of
	{ok, [{broadaddr, Ip}]} ->
	    {ok, S} = gen_udp:open(5010, [{broadcast, true}]),
	    gen_udp:send(S, Ip, 6000, IoList),
	    gen_udp:close(S);
	_ ->
	    io:format("Bad interface name, or\n"
		      "broadcasting not supported\n" )
    end.

listen() ->
    {ok, S} = gen_udp:open(6000),
    loop(S).

loop(S) ->
    receive
	Any ->
	    io:format("received:~p~n" , [Any]),
	    loop(S)
    end.
