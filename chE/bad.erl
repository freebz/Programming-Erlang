-module(bad).

% 헤드 불일치

foo(1,2) ->
    a;
foo(2,3,a) ->
    b.


% 언바운드 변수

foo(A, B) ->
    bar(A, dothis(X), B),
    baz(Y, X).


% 불안전한 변수

foo() ->
    case bar() of
	1 ->
	    X = 1,
	    Y = 2;
	2 ->
	    X = 3
    end,
    b(X).


foo() ->
    case bar() of
	1 ->
	    X = 1,
	    Y = 2;
	2 ->
	    X = 3
    end,
    b(X, Y).


% 그림자 변수

foo(X, L) ->
    lists:map(fun(X) -> 2*X end, L).


foo(X, L) ->
    lists:map(fun(Z) -> 2*Z end, L).
