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
         ensure_started/0,
         ensure_started/1,
         start/0,
         start/1,
         start_link/0,
         start_link/1,
         is_running/0,
         is_running/1,
         stop/0,
         stop/1,
         which_profiles/0
        ]).

%% Types
-export_type([profile/0]).

-ignore_xref([
              {?MODULE, config, 0},
              {?MODULE, config, 1},
              {?MODULE, config, 2},
              {?MODULE, ensure_started, 0},
              {?MODULE, ensure_started, 1},
              {?MODULE, start, 0},
              {?MODULE, start, 1},
              {?MODULE, start_link, 0},
              {?MODULE, start_link, 1},
              {?MODULE, is_running, 0},
              {?MODULE, is_running, 1},
              {?MODULE, stop, 0},
              {?MODULE, stop, 1},
              {?MODULE, which_profiles, 0}
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

-spec ensure_started() -> ok | {error, Reason} when
      Reason :: term().
ensure_started() ->
    ensure_started(minikube).

-spec ensure_started(profile()) -> ok | {error, Reason} when
      Reason :: term().
ensure_started(Profile) ->
    case start(Profile) of
        ok ->
            ok;
        {error, already_present} ->
            ok;
        {error, {already_started, _Pid}} ->
            ok;
        Error ->
            Error
    end.

-spec start() -> ok | {error, Reason} when
      Reason :: term().
start() ->
    start(minikube).

-spec start(profile()) -> ok | {error, Reason} when
      Reason :: term().
start(Profile) ->
    case minikube_sup:add_profile(Profile) of
        {ok, _Pid} ->
            minikube_driver:start(Profile);
        {ok, _Pid, _Opts} ->
            minikube_driver:start(Profile);
        Error ->
            Error
    end.

-spec start_link() -> ok | {error, Reason} when
      Reason :: term().
start_link() ->
    start_link(minikube).

-spec start_link(profile()) -> ok | {error, Reason} when
      Reason :: term().
start_link(Profile) ->
    Result = minikube_driver:start_link(Profile),
    ok = minikube_driver:start(Profile),
    Result.

-spec is_running() -> boolean() | {error, Reason} when
      Reason :: term().
is_running() ->
    is_running(minikube).

-spec is_running(profile()) -> boolean() | {error, Reason} when
      Reason :: term().
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

-spec stop() -> ok | {error, Reason} when
      Reason :: term().
stop() ->
    stop(minikube).

-spec stop(profile()) -> ok | {error, Reason} when
      Reason :: term().
stop(Profile) ->
    minikube_driver:stop(Profile),
    minikube_sup:remove_profile(Profile).

-spec which_profiles() -> [profile()].
which_profiles() ->
    Children = supervisor:which_children(minikube_sup),
    [
     Profile ||
     {Profile, Child, _Type, _Module} <- Children,
     is_pid(Child)
    ].

%%====================================================================
%% Internal functions
%%====================================================================
