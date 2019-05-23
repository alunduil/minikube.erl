-module(minikube_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
         suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2,
         groups/0,
         all/0
        ]).

-export([
         idempotent_ensure_started/0,
         idempotent_ensure_started/1,
         nonidempotent_start/0,
         nonidempotent_start/1,
         start_then_stop/0,
         start_then_stop/1,
         no_profiles/0,
         no_profiles/1,
         sentinel_profile/0,
         sentinel_profile/1,
         not_started_status/0,
         not_started_status/1,
         started_status/0,
         started_status/1
        ]).

suite() ->
    [{timestamp, {seconds, 30}}].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(minikube),
    Config.

end_per_suite(_Config) ->
    application:stop(minikube).

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    lists:foreach(fun minikube:stop/1, minikube:which_profiles()),
    ok.

groups() ->
    [
     {ensure_started, [], [idempotent_ensure_started]},
     {start, [], [nonidempotent_start]},
     {stop, [{repeat, 3}], [start_then_stop]},
     {which_profiles, [], [no_profiles, sentinel_profile]},
     {status, [], [not_started_status, started_status]}
    ].

all() ->
    [
     {group, ensure_started},
     {group, start},
     {group, stop},
     {group, which_profiles},
     {group, status}
    ].

%%--------------------------------------------------------------------

idempotent_ensure_started() ->
    [].

idempotent_ensure_started(_Config) ->
    ok = minikube:ensure_started(sentinel),
    ok = minikube:ensure_started(sentinel).

nonidempotent_start() ->
    [].

nonidempotent_start(_Config) ->
    ok = minikube:start(sentinel),
    {error, {already_started, _Pid}} = minikube:start(sentinel).

start_then_stop() ->
    [].

start_then_stop(_Config) ->
    ok = minikube:start(sentinel),
    ok = minikube:stop(sentinel).

no_profiles() ->
    [].

no_profiles(_Config) ->
    0 = length(minikube:which_profiles()).

sentinel_profile() ->
    [].

sentinel_profile(_Config) ->
    ok = minikube:ensure_started(sentinel),
    [sentinel] = minikube:which_profiles().

not_started_status() ->
    [].

not_started_status(_Config) ->
    {error, not_found} = minikube:is_running(sentinel).

started_status() ->
    [].

started_status(_Config) ->
    ok = minikube:ensure_started(sentinel),
    true = minikube:is_running(sentinel).
