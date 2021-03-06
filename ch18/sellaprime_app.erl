-module(sellaprime_app).
-behaviour(application).
-export([start/2, stop/1]).

%%-----------------------------------------------------------------------------
%% 함수: start(Type, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%% 설명: 이 함수는 애플리케이션이 application/1,2를 사용하여 시작될 때 호출되며,
%% 애플리케이션의 프로세스를 시작한다. 애플리케이션이 OTP 설계 원칙에 입각해서 슈퍼비전
%% 트리로 구성된 경우라면, 트리의 최상위 슈퍼바이저를 시작한다는 의미다.
%%-----------------------------------------------------------------------------

start(_Type, StartArgs) ->
    sellaprime_supervisor:start_link(StartArgs).

%%-----------------------------------------------------------------------------
%% 함수: stop(State) -> void()
%% 설명: 이 함수는 애플리케이션이 중지했을 때 호출된다.
%% 이 함수는 Module:start/2와 반대되는 의미를 가지며, 모든 필요한 정리 작업을
%% 수행한다. 반환 값은 무시된다.
%%-----------------------------------------------------------------------------

stop(_State) ->
    ok.
