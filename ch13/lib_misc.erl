-module(lib_misc).
-import(lists, [map/2, sort/1]).
-export([consult/1, unconsult/2, ls/1]).

consult(File) ->
    case file:open(File, read) of
	{ok, S} ->
	    Val = consult1(S),
	    file:close(S),
	    {ok, Val};
	{error, Why} ->
	    {error, Why}
    end.

consult1(S) ->
    case io:read(S, '') of
	{ok, Term} -> [Term|consult1(S)];
	eof        -> [];
	Error      -> Error
    end.


unconsult(File, L) ->
    {ok, S} = file:open(File, write),
    lists:foreach(fun(X) -> io:format(S, "~p.~n" ,[X]) end, L),
    file:close(S).


-include_lib("kernel/include/file.hrl").
file_size_and_type(File) ->
    case file:read_file_info(File) of
	{ok, Facts} ->
	    {Facts#file_info.type, Facts#file_info.size};
	_ ->
	    error
    end.


ls(Dir) ->
    {ok, L} = file:list_dir(Dir),
    map(fun(I) -> {I, file_size_and_type(I)} end, sort(L)).
