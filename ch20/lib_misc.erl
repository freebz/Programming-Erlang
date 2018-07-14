-module(lib_misc).
-export([pmap/2, pmap1/2,
	 foreachWordInFile/2]).
-import(lists, [reverse/1, reverse/2,
		map/2, foreach/2]).

pmap(F, L) ->
    S = self(),
    %% make_ref() 는 유일한 참조를 반환한다
    %% 나중에 매치할 것이다.
    Ref = erlang:make_ref(),
    Pids = map(fun(I) ->
		       spawn(fun() -> do_f(S, Ref, F, I) end)
	       end, L),
    %% 결과를 모은다
    gather(Pids, Ref).
do_f(Parent, Ref, F, I) ->
    Parent ! {self(), Ref, (catch F(I))}.

gather([Pid|T], Ref) ->
    receive
	{Pid, Ref, Ret} -> [Ret|gather(T, Ref)]
    end;
gather([], _) ->
    [].


pmap1(F, L) ->
    S = self(),
    Ref = erlang:make_ref(),
    foreach(fun(I) ->
		    spawn(fun() -> do_f1(S, Ref, F, I) end)
	    end, L),
    %% 결과를 모은다
    gather1(length(L), Ref, []).

do_f1(Parent, Ref, F, I) ->
    Parent ! {Ref, (catch F(I))}.

gather1(0, _, L) -> L;
gather1(N, Ref, L) -> 
    receive
	{Ref, Ret} -> gather1(N-1, Ref, [Ret|L])
    end.


%% evalute F(Word) for each word in the file File
foreachWordInFile(File, F) ->
    case file:read_file(File) of
	{ok, Bin} -> foreachWordInString(binary_to_list(Bin), F);
	_         -> void
    end.

foreachWordInString(Str, F) ->
    case get_word(Str) of
	no ->
	    void;
	{Word, Str1} ->
	    F(Word),
	    foreachWordInString(Str1, F)
    end.

isWordChar(X) when $A=< X, X=<$Z -> true;
isWordChar(X) when $0=< X, X=<$9 -> true;
isWordChar(X) when $a=< X, X=<$z -> true;
isWordChar(_)  -> false.
 
get_word([H|T]) ->
    case isWordChar(H) of
	true  -> collect_word(T, [H]);
	false -> get_word(T)
    end;
get_word([]) ->
    no.

collect_word([H|T]=All, L) ->
    case isWordChar(H) of
	true  -> collect_word(T, [H|L]);
	false -> {reverse(L), All}
    end;
collect_word([], L) ->
    {reverse(L), []}.
