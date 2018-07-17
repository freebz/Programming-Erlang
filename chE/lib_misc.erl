deliberate_error(A) ->
    bad_function(A, 12),
    lists:reverse(A).

bad_function(A, _) ->
    {ok, Bin} = file:open({abc,123}, A),
    binary_to_list(Bin).


deliberate_error1(A) ->
    bad_function(A, 12).


-define(NYI(X),(begin
		    io:format("*** NYI ~p ~p ~p~n" ,[?MODULE, ?LINE, X]),
		    exit(nyi)
		end)).


glurk(X, Y) ->
    ?NYI({glurk, X, Y}).


dump(File, Term) ->
    Out = File ++ ".tmp" ,    
    io:format("** dumping to ~s~n" ,[Out]),
    {ok, S} = file:open(Out, [write]),
    io:format(S, "~p.~n" ,[Term]),
    file:close(S).
