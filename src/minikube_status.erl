%%%-------------------------------------------------------------------
%% @doc minikube status.
%% @end
%%%-------------------------------------------------------------------

-module(minikube_status).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([is_running/1, parse/1]).

-export_type([type/0]).

%%====================================================================
%% Types
%%====================================================================

-record(status, {
          host :: undefined | running | stopped,
          kubelet :: undefined | running,
          apiserver :: undefined | running | stopped,
          kubectl :: undefined | {configured, inet:ip_address()}
         }).

-opaque type() :: #status{}.

%%====================================================================
%% API functions
%%====================================================================

-spec is_running(type()) -> boolean().
is_running(#status{host=running}) ->
  true;
is_running(#status{kubelet=running}) ->
  true;
is_running(#status{apiserver=running}) ->
  true;
is_running(_Status) ->
  false.

-spec parse(string()) -> {ok, type()} | {error, term()}.
parse(Input) ->
  parse(string:lexemes(Input, [$\r, $\n]), #status{}).

%%====================================================================
%% Internal functions
%%====================================================================

-spec parse([string()], type()) -> Result when
    Result :: {ok, type()} | {error, term()}.
parse(["host: " ++ Value|Rest], Status) ->
  case Value of
    "" ->
      parse(Rest, Status#status{host=undefined});
    "Stopped" ->
      parse(Rest, Status#status{host=stopped});
    "Running" ->
      parse(Rest, Status#status{host=running});
    Other ->
      {error, {invalid, {host, Other}}}
  end;
parse(["kubelet: " ++ Value|Rest], Status) ->
  case Value of
    "" ->
      parse(Rest, Status#status{kubelet=undefined});
    "Running" ->
      parse(Rest, Status#status{kubelet=running});
    Other ->
      {error, {invalid, {kubelet, Other}}}
  end;
parse(["apiserver: " ++ Value|Rest], Status) ->
  case Value of
    "" ->
      parse(Rest, Status#status{apiserver=undefined});
    "Stopped" ->
      parse(Rest, Status#status{apiserver=stopped});
    "Running" ->
      parse(Rest, Status#status{apiserver=running});
    Other ->
      {error, {invalid, {apiserver, Other}}}
  end;
parse(["kubectl: " ++ Value|Rest], Status) ->
  case Value of
    "" ->
      parse(Rest, Status#status{kubectl=undefined});
    "Correctly Configured: pointing to minikube-vm at " ++ Address ->
      {ok, IP} = inet:parse_address(Address),
      parse(Rest, Status#status{kubectl={configured, IP}});
    Other ->
      {error, {invalid, {apiserver, Other}}}
  end;
parse([Line|_Rest], _Status) ->
  {error, {invalid, {line, Line}}};
parse([], Status) ->
  {ok, Status}.

-ifdef(EUNIT).
-spec parse_test_() -> [fun(() -> term())].
parse_test_() ->
  Expected1 = #status{
                 host=stopped,
                 kubelet=undefined,
                 apiserver=undefined,
                 kubectl=undefined
                },
  Input1 = [
            "host: Stopped",
            "kubelet: ",
            "apiserver: ",
            "kubectl: "
           ],

  Expected2 = #status{
                 host=running,
                 kubelet=running,
                 apiserver=running,
                 kubectl={configured, {192, 168, 39, 103}}
                },
  Input2 = [
            "host: Running",
            "kubelet: Running",
            "apiserver: Running",
            % Intentional implicit string concatenation.
            "kubectl: Correctly Configured: pointing to minikube-vm at "
            "192.168.39.103"
           ],

  [
    ?_assertEqual({ok, Expected1}, parse(Input1, #status{})),
    ?_assertEqual({ok, Expected2}, parse(Input2, #status{}))
  ].
-endif.
