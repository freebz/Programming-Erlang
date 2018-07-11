-module(event_handler).
-export([make/1, add_handler/2, event/2]).

%% Name이라는 새 이벤트 핸들러를 만든다
%% 헨들러 함수는 noOp다 -- 따라서 이벤트로 아무것도 하지 않는다
make(Name) ->
    register(Name, spawn(fun() -> my_handler(fun no_op/1) end)).

add_handler(Name, Fun) -> Name ! {add, Fun}.

%% 이벤트를 발생시킨다
event(Name, X) -> Name ! {event, X}.

my_handler(Fun) ->
    receive
	{add, Fun1} ->
	    my_handler(Fun1);
	{event, Any} ->
	    (catch Fun(Any)),
	    my_handler(Fun)
    end.

no_op(_) -> void.
