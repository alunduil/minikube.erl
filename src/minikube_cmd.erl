%%%-------------------------------------------------------------------
%% @doc minikube commands.
%% @end
%%%-------------------------------------------------------------------

-module(minikube_cmd).

-include_lib("kernel/include/logger.hrl").

-behaviour(minikube_driver).

%% API
-export([start/1, status/1, stop/1]).

%%====================================================================
%% API functions
%%====================================================================

-spec start(Profile::string()) -> ok.
start(Profile) ->
    {ok, Driver} = application:get_env(vm_driver),
    ?LOG_DEBUG(#{vm_driver => Driver}),
    os:cmd(minikube_cmd(Profile, atom_to_list(Driver)) ++ " start"),
    ok.

-spec status(string()) -> {ok, minikube_status:type()} | {error, Reason} when
      Reason :: term().
status(Profile) ->
    minikube_status:parse(os:cmd(minikube_cmd(Profile) ++ " status")).

-spec stop(string()) -> ok.
stop(Profile) ->
    os:cmd(minikube_cmd(Profile) ++ " stop"),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

-spec minikube_cmd(Profile::string(), Driver::string()) -> string().
minikube_cmd(Profile, Driver) ->
    minikube_cmd(Profile) ++ " --vm-driver " ++ Driver.

-spec minikube_cmd(Profile::string()) -> string().
minikube_cmd(Profile) ->
    minikube_cmd() ++ " --profile " ++ Profile.

-spec minikube_cmd() -> string().
minikube_cmd() ->
    "minikube".
