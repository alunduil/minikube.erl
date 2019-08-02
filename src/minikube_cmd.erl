%%%-------------------------------------------------------------------
%% @doc minikube commands.
%% @end
%%%-------------------------------------------------------------------

-module(minikube_cmd).

-include_lib("kernel/include/logger.hrl").

%% API
-export([start/1, status/1, stop/1]).

%%====================================================================
%% API functions
%%====================================================================

-spec start(Profile :: string()) -> ok.
start(Profile) ->
  os:cmd(string:join([minikube_cmd(Profile), "start"], " ")),
  ok.

-spec status(string()) -> Result when
    Result :: {ok, minikube_status:type()} | {error, term()}.
status(Profile) ->
  minikube_status:parse(os:cmd(string:join([minikube_cmd(Profile), "status"], " "))).

-spec stop(string()) -> ok.
stop(Profile) ->
  os:cmd(string:join([minikube_cmd(Profile), "stop"], " ")),
  ok.

%%====================================================================
%% Internal functions
%%====================================================================

-spec minikube_cmd(Profile :: string()) -> string().
minikube_cmd(Profile) ->
  string:join([minikube_cmd(), "--profile", Profile], " ").

-spec minikube_cmd() -> string().
minikube_cmd() ->
  "minikube".
