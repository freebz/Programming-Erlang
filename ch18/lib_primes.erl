%% 적어도 K 자리의 소수를 만듦
%% 여기서는 '베르트낭 공준(Bertrand's postulate)'을 사용함
%% 베르트낭 공준은 N>3인 모든 N에 대하여,
%% N<P<2N-2를 만족하는 소수 P가 존재한다는 것임
%% 이것은 1850년 체비세프(Tchebychef)에 의해 증명됨
%% (에어디쉬(Erdos)가 1932년에 증명을 보강함)

-module(lib_primes).
-export([make_prime/1, is_prime/1, make_random_int/1]).

make_prime(1) ->
    lists:nth(random:uniform(5), [1,2,3,5,7]);
make_prime(K) when K > 0 ->
    new_seed(),
    N = make_random_int(K),
    if N > 3 ->
	    io:format("Generating a ~w digit prime " ,[K]),
	    MaxTries = N - 3,
	    P1 = make_prime(MaxTries, N+1),
	    io:format("~n" ,[]),
	    P1;
       true ->
	    make_prime(K)
    end.
make_prime(0, _) ->
    exit(impossible);
make_prime(K, P) ->
    io:format("." ,[]),
    case is_prime(P) of
	true -> P;
	false -> make_prime(K-1, P+1)
    end.

%% 페르마(Fermat) 소정리에 의하면
%% N이 소수이고 A<N이면
%% A^N mode N = A

is_prime(D) ->
    new_seed(),
    is_prime(D, 100).

is_prime(D, Ntests) ->
    N = length(integer_to_list(D)) -1,
    is_prime(Ntests, D, N).

is_prime(0, _, _) -> true;
is_prime(Ntest, N, Len) -> 
    K = random:uniform(Len),
    %% A는 N보다 적은 난수
    A = make_random_int(K),
    if
	A < N ->
	    case lib_lin:pow(A,N,N) of
		A -> is_prime(Ntest-1,N,Len);
		_ -> false
	    end;
	true ->
	    is_prime(Ntest, N, Len)
    end.


%% make_random_int(N) -> a random integer with N digits.
make_random_int(N) -> new_seed(), make_random_int(N, 0).

make_random_int(0, D) -> D;
make_random_int(N, D) -> 
    make_random_int(N-1, D*10 + (random:uniform(10)-1)).


new_seed() ->
    {_,_,X} = erlang:now(),
    {H,M,S} = time(),
    H1 = H * X rem 32767,
    M1 = M * X rem 32767,
    S1 = S * X rem 32767,
    put(random_seed, {H1,M1,S1}).
