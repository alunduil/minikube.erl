%%%-------------------------------------------------------------------
%% @doc minikube driver.
%% @end
%%%-------------------------------------------------------------------

-module(minikube_driver).

-include_lib("kernel/include/logger.hrl").

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

%%====================================================================
%% API functions
%%====================================================================

-spec start(atom()) -> ok | {error, term()}.
start(Profile) ->
  gen_statem:call(Profile, start).

-spec status(atom()) -> Result when
    Result :: {ok, Status} | {error, Reason},
    Status ::  minikube_status:type(),
    Reason :: term().
status(Profile) ->
  gen_statem:call(Profile, status).

-spec stop(atom()) -> ok | {error, term()}.
stop(Profile) ->
  gen_statem:call(Profile, stop).

%%--------------------------------------------------------------------

start_link(Profile) ->
  gen_statem:start_link({local, Profile}, ?MODULE, [Profile], []).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================

init([Profile]) ->
  case minikube_cmd:status(atom_to_list(Profile)) of
    {ok, Status} ->
      {ok, Status, atom_to_list(Profile)};
    {error, Reason} ->
      {stop, Reason}
  end.

callback_mode() ->
  handle_event_function.

handle_event({call, From}, start, _Status, Profile) ->
  minikube_cmd:start(Profile),
  case minikube_cmd:status(Profile) of
    {ok, Status} ->
      {next_state, Status, Profile, {reply, From, ok}};
    Other ->
      {keep_state_and_data, {reply, From, Other}}
  end;
handle_event({call, From}, status, _Status, Profile) ->
  case minikube_cmd:status(Profile) of
    {ok, Status} ->
      {next_state, Status, Profile, {reply, From, {ok, Status}}};
    Other ->
      {keep_state_and_data, {reply, From, Other}}
  end;
handle_event({call, From}, stop, _Status, Profile) ->
  minikube_cmd:stop(Profile),
  case minikube_cmd:status(Profile) of
    {ok, Status} ->
      {next_state, Status, Profile, {reply, From, ok}};
    Other ->
      {keep_state_and_data, {reply, From, Other}}
  end.

terminate(_Reason, _Status, Profile) ->
  minikube_cmd:stop(Profile).

%%====================================================================
%% Internal functions
%%====================================================================
