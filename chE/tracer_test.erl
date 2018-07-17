-module(tracer_test).

-include_lib("stdlib/include/ms_transform.hrl").
-compile(export_all).

test1() ->
    dbg:tracer(),
    dbg:tpl(tracer_test,fib,'_',
	    dbg:fun2ms(fun(_) -> return_trace() end)),
    dbg:p(all,[c]),
    tracer_test:fib(4).

test2() ->
    trace_module(tracer_test, fun() -> fib(4) end).

fib(0) -> 1;    
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2).

trace_module(Mod, StartFun) ->
    %% 추적을 하기 위해 프로세스를 띄움
    spawn(fun() -> trace_module1(Mod, StartFun) end).

trace_module1(Mod, StartFun) ->
    %% 다음 줄이 의미하는 것: 모든 함수 호출을
    %%                        추적하고 Mod의 값을 반환
    erlang:trace_pattern({Mod, '_','_'},
			 [{'_',[],[{return_trace}]}],
			 [local]),
    %% 추적을 하기 위해 함수를 띄움
    S = self(),
    Pid = spawn(fun() -> do_trace(S, StartFun) end),
    %% 추적을 설정. 시스템에게 프로세스 Pid에 대한 추적을 시작하라고 알림
    erlang:trace(Pid, true, [call,procs]),
    %% Pid더러 시작하라고 함.
    Pid ! {self(), start},
    trace_loop().

%% do_trace는 Parent가 그렇게 하라고 할 때 StartFun()을 평가함
do_trace(Parent, StartFun) ->
    receive
	{Parent, start} ->
	    StartFun()
    end.

%% trace_loop 는 함수 호출과 반환 값을 표시
trace_loop() ->
    receive
	{trace,_,call, X} ->
	    io:format("Call: ~p~n" ,[X]),
	    trace_loop();
	{trace,_,return_from, Call, Ret} ->
	    io:format("Return From: ~p => ~p~n" ,[Call, Ret]),
	    trace_loop();
	Other ->
	    %% 무언가 다른 메시지- 출력함
	    io:format("Other = ~p~n" ,[Other]),
	    trace_loop()
    end.
