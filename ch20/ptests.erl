-module(ptests).
-export([tests/1, fib/1]).
-import(lists, [map/2]).
-import(lib_misc, [pmap/2]).

tests([N]) ->
    Nsched = list_to_integer(atom_to_list(N)),
    run_tests(1, Nsched).

run_tests(N, Nsched) ->
    case test(N) of
	stop ->
	    init:stop();
	Val ->
	    io:format("~p.~n" ,[{Nsched, Val}]),
	    run_tests(N+1, Nsched)
    end.

test(1) ->
    %% 100개의 리스트를 만든다
    %% 각 리스트에는 1000개의 난수가 들어 있다
    seed(),
    S = lists:seq(1,100),
    L = map(fun(_) -> mkList(1000) end, S),
    {Time1, S1} = timer:tc(lists,    map,  [fun lists:sort/1, L]),
    {Time2, S2} = timer:tc(lib_misc, pmap, [fun lists:sort/1, L]),
    {sort, Time1, Time2, equal(S1, S2)};
test(2) ->
    %% L = [27,27,27,...] 100회
    L = lists:duplicate(100, 27),
    {Time1, S1} = timer:tc(lists,    map,  [fun ptests:fib/1, L]),
    {Time2, S2} = timer:tc(lib_misc, pmap, [fun ptests:fib/1, L]),
    {fib, Time1, Time2, equal(S1, S2)};
test(3) ->
    stop.

%% Equal 은 map과 pmap이 동일한 것을 계산하는지 검사하는 데 사용됨
equal(S,S) -> true;
equal(S1,S2) -> {differ, S1, S2}.

%% 재귀적(비효율적) 피보나치
fib(0) -> 1;    
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2).

%% 난수 생성기를 초기화한다. 이렇게 하는 이유는
%% 프로그램 실행 시마다 동일한 난수 순서를 얻기 위함임

seed() -> random:seed(44,55,66).

%% 난수 K개의 리스트를 만듦.
%% 각 난수는 1..1000000의 범위
mkList(K) -> mkList(K, []).

mkList(0, L) -> L;
mkList(N, L) -> mkList(N-1, [random:uniform(1000000)|L]).
