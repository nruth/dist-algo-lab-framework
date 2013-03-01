-module(myapp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-ifdef(TEST). %ifdef to prevent test-code compilation into ebin
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    myapp_sup:start_link().

stop(_State) ->
    ok.

-ifdef(TEST). %ifdef to prevent test-code compilation into ebin

simple_test() ->
    % the ok binding here is a common Erlang idiom which says the RHS must equal ok, else there will be a runtime error
    % it is not a variable binding. Recall lower-case terms are atoms not
    % variables. Variables start with upper-case characters, e.g. Ok
    ok = application:start(myapp),
    ?assertNot(undefined == whereis(myapp_sup)).
-endif.
