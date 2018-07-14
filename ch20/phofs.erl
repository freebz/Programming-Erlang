-module(phofs).
-export([mapreduce/4]).

-import(lists, [foreach/2]).

%% F1(Pid, X) -> {Key,Val} 를 Pid로 보낸다
%% F2(Key, [Val], AccIn) -> AccOut

mapreduce(F1, F2, Acc0, L) ->
    S = self(),
    Pid = spawn(fun() -> reduce(S, F1, F2, Acc0, L) end),
    receive
	{Pid, Result} ->
	    Result
    end.

reduce(Parent, F1, F2, Acc0, L) ->
    process_flag(trap_exit, true),
    ReducePid = self(),
    %% Map 프로세스를 생성한다
    %% L의 각 요소 X에 대해 하나씩
    foreach(fun(X) ->
		    spawn_link(fun() -> do_job(ReducePid, F1, X) end)
	    end, L),
    N = length(L),
    %% 키를 저장할 사전을 만든다
    Dict0 = dict:new(),
    %% N개의 Map프로세스가 종료하기를 기다린다
    Dict1 = collect_replies(N, Dict0),
    Acc = dict:fold(F2, Acc0, Dict1),
    Parent ! {self(), Acc}.

%% collect_replies(N, Dict)
%%     N 프로세스로부터 {Key,Value} 메시지를 수집하고 병합한다
%%     N개의 프로세스가 종료하면 {Key,[Value]} 쌍의 사전을 반환한다.

collect_replies(0, Dict) ->
    Dict;
collect_replies(N, Dict) ->
    receive
	{Key, Val} ->
	    case dict:is_key(Key, Dict) of
		true ->
		    Dict1 = dict:append(Key, Val, Dict),
		    collect_replies(N, Dict1);
		false ->
		    Dict1 = dict:store(Key,[Val], Dict),
		    collect_replies(N, Dict1)
	    end;
	
	{'Exit', _, Why} ->
	    collect_replies(N-1, Dict)
    end.

%% F(Pid, X)를 호출한다
%%   F는 Pid로 {Key,Value} 메시지를 보내고 종료해야 한다

do_job(ReducePid, F, X) ->
    F(ReducePid, X).
