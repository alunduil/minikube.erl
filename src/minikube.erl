%%%-------------------------------------------------------------------
%% @doc minikube API.
%% @end
%%%-------------------------------------------------------------------

-module(minikube).

%% API
-export([
         config/0,
         config/1,
         config/2,
         start_link/0,
         start_link/1,
         ensure_started/0,
         ensure_started/1,
         is_running/0,
         is_running/1,
         stop/0,
         stop/1
        ]).

%% Types
-export_type([profile/0]).

-ignore_xref([
              {?MODULE, config, 0},
              {?MODULE, config, 1},
              {?MODULE, config, 2},
              {?MODULE, start_link, 0},
              {?MODULE, start_link, 1},
              {?MODULE, ensure_started, 0},
              {?MODULE, ensure_started, 1},
              {?MODULE, is_running, 0},
              {?MODULE, is_running, 1},
              {?MODULE, stop, 0},
              {?MODULE, stop, 1}
             ]).

%%====================================================================
%% Types
%%====================================================================

-type profile() :: atom().

%%====================================================================
%% API functions
%%====================================================================

-spec config() -> [proplists:property()].
config() ->
  minikube_config:view().

-spec config(atom()) -> string().
config(Name) ->
  minikube_config:get(Name).

-spec config(Name, Value) -> ok when
    Name :: atom(),
    Value :: string().
config(Name, Value) ->
  minikube_config:set(Name, Value).

-spec start_link() -> Result when
    Result :: {ok, Pid} | ignore | {error, Error},
    Pid :: pid(),
    Error :: {already_started, Pid} | term().
start_link() ->
  start_link(minikube).

-spec start_link(profile()) -> Result when
    Result :: {ok, Pid} | ignore | {error, Error},
    Pid :: pid(),
    Error :: {already_started, Pid} | term().
start_link(Profile) ->
  minikube_driver:start_link(Profile).

-spec ensure_started() -> ok | {error, term()}.
ensure_started() ->
  ensure_started(minikube).

-spec ensure_started(profile()) -> ok | {error, term()}.
ensure_started(Profile) ->
  case is_running(Profile) of
    false ->
      minikube_driver:start(Profile);
    true ->
      ok;
    Other ->
      Other
  end.

-spec is_running() -> boolean() | {error, term()}.
is_running() ->
  is_running(minikube).

-spec is_running(profile()) -> boolean() | {error, term()}.
is_running(Profile) ->
  try minikube_driver:status(Profile) of
    {ok, Status} ->
      minikube_status:is_running(Status);
    Error ->
      Error
  catch
    exit:{noproc, _Details} ->
      {error, not_found}
  end.

-spec stop() -> ok | {error, term()}.
stop() ->
  stop(minikube).

-spec stop(profile()) -> ok | {error, term()}.
stop(Profile) ->
  minikube_driver:stop(Profile).

%%====================================================================
%% Internal functions
%%====================================================================
