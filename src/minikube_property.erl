%%%-------------------------------------------------------------------
%% @doc minikube property.
%% @end
%%%-------------------------------------------------------------------

-module(minikube_property).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([from_atom/1, to_atom/1]).

%%====================================================================
%% API functions
%%====================================================================

-spec from_atom(atom()) -> string().
from_atom(vm_driver) ->
    "vm-driver";
from_atom(profile) ->
    "profile";
from_atom(Atom) ->
    camel_case(atom_to_list(Atom)).

-spec to_atom(string()) -> atom().
to_atom("vm-driver") ->
    vm_driver;
to_atom("profile") ->
    profile;
to_atom(String) ->
    list_to_atom(snake_case(String)).

%%====================================================================
%% Internal functions
%%====================================================================

-spec camel_case(string()) -> string().
camel_case(Input) ->
    string:join([uppercase_first(Lexeme)||Lexeme <- string:lexemes(Input, "_")], "").

-ifdef(EUNIT).
-spec camel_case_test_() -> [fun(() -> term())].
camel_case_test_() ->
    [
     ?_assertEqual("", camel_case("")),
     ?_assertEqual("Foo", camel_case("foo")),
     ?_assertEqual("FooBar", camel_case("foo_bar")),
     ?_assertEqual("Foo", camel_case("Foo")),
     ?_assertEqual("FooBar", camel_case("FooBar"))
    ].
-endif.

-spec uppercase_first(string()) -> string().
uppercase_first([]) ->
    "";
uppercase_first([C|Rest]) ->
    string:join([string:uppercase([C]), Rest], "").

-ifdef(EUNIT).
-spec uppercase_first_test_() -> [fun(() -> term())].
uppercase_first_test_() ->
    [
     ?_assertEqual("", uppercase_first("")),
     ?_assertEqual("Foo", camel_case("foo")),
     ?_assertEqual("Foo", camel_case("Foo"))
    ].
-endif.

-spec snake_case(string()) -> string().
snake_case(Input) ->
    string:trim(snake_case(Input, ""), both, "_").

-ifdef(EUNIT).
-spec snake_case_test_() -> [fun(() -> term())].
snake_case_test_() ->
    [
     ?_assertEqual("", snake_case("")),
     ?_assertEqual("foo", snake_case("foo")),
     ?_assertEqual("foo_bar", snake_case("foo_bar")),
     ?_assertEqual("foo", snake_case("Foo")),
     ?_assertEqual("foo_bar", snake_case("FooBar"))
    ].
-endif.

-spec snake_case(Input, Accumulator) -> Output when
      Input :: string(),
      Accumulator :: string(),
      Output :: string().
snake_case("", Accumulator) ->
    Accumulator;
snake_case([C|Rest], Accumulator) when $A =< C andalso C =< $Z ->
    snake_case(Rest, string:join([Accumulator, [$_], string:lowercase([C])], ""));
snake_case([C|Rest], Accumulator) ->
    snake_case(Rest, string:join([Accumulator, string:lowercase([C])], "")).
