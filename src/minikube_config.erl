%%%-------------------------------------------------------------------
%% @doc minikube config.
%% @end
%%%-------------------------------------------------------------------

-module(minikube_config).

-include_lib("kernel/include/logger.hrl").

%% API
-export([get/1, set/2, view/0]).

%%====================================================================
%% API functions
%%====================================================================

-spec get(Name::atom()) -> string().
get(Name) ->
    ?LOG_INFO(#{action => get, name => Name}),
    os:cmd(minikube_config_cmd(get) ++ " " ++ minikube_property:from_atom(Name)).

-spec set(Name::atom(), Value::string()) -> ok.
set(Name, Value) ->
    ?LOG_INFO(#{action => set, name => Name, value => Value}),
    os:cmd(minikube_config_cmd(set) ++ " " ++ minikube_property:from_atom(Name) ++ " " ++ Value),
    ok.

-spec view() -> [{atom(), string()}].
view() ->
    ?LOG_INFO(#{action => view}),
    parse_view(os:cmd(minikube_config_cmd(view))).

%%====================================================================
%% Internal functions
%%====================================================================

-spec minikube_config_cmd(SubCommand::atom()) -> string().
minikube_config_cmd(get) ->
    minikube_config_cmd() ++ " get";
minikube_config_cmd(set) ->
    minikube_config_cmd() ++ " set";
minikube_config_cmd(view) ->
    minikube_config_cmd() ++ " view".

-spec minikube_config_cmd() -> string().
minikube_config_cmd() ->
    "minikube config".

-spec parse_view(string()) -> [proplists:property()].
parse_view(Input) ->
    [Document] = yamerl:decode(Input),
    [{minikube_property:to_atom(Name), Value}||{Name, Value} <- Document].
