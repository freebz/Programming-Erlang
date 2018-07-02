-module(lib_misc).
-export([sleep/1]).

sleep(T) ->
    receive
    after T ->
        true
    end.
