%%%-------------------------------------------------------------------
%% @doc minikube top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(minikube_sup).

-include_lib("kernel/include/logger.hrl").

-behaviour(supervisor).

%% API
-export([add_profile/1, remove_profile/1, start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

-spec add_profile(atom()) -> supervisor:startchild_ret().
add_profile(Profile) ->
    ?LOG_INFO(#{actio => start, profile => Profile}),
    ChildSpec = #{
      id => Profile,
      start => {minikube_driver, start_link, [Profile]},
      restart => transient
     },
    supervisor:start_child(?MODULE, ChildSpec).

-spec remove_profile(atom()) -> supervisor:startchild_ret().
remove_profile(Profile) ->
    ?LOG_INFO(#{action => stop, profile => Profile}),
    supervisor:terminate_child(?MODULE, Profile),
    supervisor:delete_child(?MODULE, Profile).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

-spec init([]) -> 'ignore' | {'ok', Spec} when
      Spec :: {supervisor:sup_flags(), [supervisor:child_spec()]}.
init([]) ->
    ?LOG_INFO(#{action => start}),
    {ok, {#{}, []}}.

%%====================================================================
%% Internal functions
%%====================================================================
