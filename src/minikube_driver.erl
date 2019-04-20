%%%-------------------------------------------------------------------
%% @doc minikube driver.
%% @end
%%%-------------------------------------------------------------------

-module(minikube_driver).

-behaviour(gen_statem).

%% API
-export([start/1, status/1, stop/1, start_link/1]).

%% gen_fsm callbacks
-export([
         init/1,
         callback_mode/0,
         handle_event/4,
         terminate/3
        ]).

-ignore_xref([{?MODULE, start_link, 1}, {?MODULE, behaviour_info, 1}]).

%%====================================================================
%% Types
%%====================================================================

-record(minikube_driver_data, {cmd_module :: module(), profile :: string()}).

%%====================================================================
%% Callbacks for minikube_drivers.
%%====================================================================

-callback start(string()) -> ok.
-callback status(string()) -> {ok, Status} | {error, Reason} when
      Status :: minikube_status:type(),
      Reason :: term().
-callback stop(string()) -> ok.

%%====================================================================
%% API functions
%%====================================================================

-spec start(atom()) -> ok | {error, Reason} when
      Reason :: term().
start(Profile) ->
    gen_statem:call(Profile, start).

-spec status(atom()) -> {ok, minikube_status:type()} | {error, Reason} when
      Reason :: term().
status(Profile) ->
    gen_statem:call(Profile, status).

-spec stop(atom()) -> ok | {error, Reason} when
      Reason :: term().
stop(Profile) ->
    gen_statem:call(Profile, stop).

%%--------------------------------------------------------------------

start_link(Profile) ->
    gen_statem:start_link({local, Profile}, ?MODULE, [Profile], []).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================

init([Profile]) ->
    CmdModule = application:get_env(minikube, cmd_module, minikube_cmd),
    case CmdModule:status(atom_to_list(Profile)) of
        {ok, Status} ->
            Data = #minikube_driver_data{
                      cmd_module=CmdModule,
                      profile=atom_to_list(Profile)
                     },
            {ok, Status, Data};
        {error, Reason} ->
            {stop, Reason}
    end.

callback_mode() -> handle_event_function.

handle_event(
  {call, From},
  start,
  _Status,
  #minikube_driver_data{cmd_module=CmdModule, profile=Profile}=Data
 ) ->
    CmdModule:start(Profile),
    case CmdModule:status(Profile) of
        {ok, Status} ->
            {next_state, Status, Data, {reply, From, ok}};
        Other ->
            {keep_state_and_data, {reply, From, Other}}
    end;
handle_event(
  {call, From},
  status,
  _Status,
  #minikube_driver_data{cmd_module=CmdModule, profile=Profile}=Data
 ) ->
    case CmdModule:status(Profile) of
        {ok, Status} ->
            {next_state, Status, Data, {reply, From, {ok, Status}}};
        Other ->
            {keep_state_and_data, {reply, From, Other}}
    end;
handle_event(
  {call, From},
  stop,
  _Status,
  #minikube_driver_data{cmd_module=CmdModule, profile=Profile}=Data
 ) ->
    CmdModule:stop(Profile),
    case CmdModule:status(Profile) of
        {ok, Status} ->
            {next_state, Status, Data, {reply, From, ok}};
        Other ->
            {keep_state_and_data, {reply, From, Other}}
    end.

terminate(
  _Reason,
  Status,
  #minikube_driver_data{cmd_module=CmdModule, profile=Profile}
 ) ->
    case minikube_status:is_running(Status) of
        true ->
            CmdModule:stop(Profile),
            ok;
        false ->
            ok
    end;
terminate(_Reason, _Status, _Data) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
