-module(lib_lin).
-export([pow/3]).

pow(A, 1, M) ->
    A rem M;
pow(A, 2, M) ->
    A*A rem M;
pow(A, B, M) ->
    B1 = B div 2,
    B2 = B - B1,
    %% B2 = B1 or B1+1
    P = pow(A, B1, M),
    case B2 of
	B1 -> (P*P) rem M;
	_ -> (P*P*A) rem M
    end.
