-module(minikube_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

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
         ensure_started/0,
         ensure_started/1,
         assert_stopped/0,
         assert_stopped/1,
         not_started_status/0,
         not_started_status/1
        ]).

suite() ->
  [{timestamp, {seconds, 30}}].

init_per_suite(Config) ->
  Config.

end_per_suite(_Config) ->
  ok.

init_per_group(_GroupName, Config) ->
  Config.

end_per_group(_GroupName, _Config) ->
  ok.

init_per_testcase(_TestCase, Config) ->
  Config.

end_per_testcase(_TestCase, _Config) ->
  ok.

groups() ->
  [
   {start_link, [], [ensure_started, assert_stopped]},
   {status, [shuffle], [not_started_status]}
  ].

all() ->
  [
   {group, start_link},
   {group, status}
  ].

%%--------------------------------------------------------------------

ensure_started() ->
  [].

ensure_started(_Config) ->
  ?assertMatch({ok, _Pid}, minikube:start_link(sentinel)),
  ?assertMatch(ok, minikube:ensure_started(sentinel)),
  ?assertMatch(true, minikube:is_running(sentinel)),
  ?assertMatch(ok, minikube:stop(sentinel)).

assert_stopped() ->
  [].

assert_stopped(_Config) ->
  ?assertMatch({ok, _Pid}, minikube:start_link(sentinel)),
  ?assertMatch(false, minikube:is_running(sentinel)).

not_started_status() ->
  [].

not_started_status(_Config) ->
  ?assertMatch({error, not_found}, minikube:is_running(sentinel)).
