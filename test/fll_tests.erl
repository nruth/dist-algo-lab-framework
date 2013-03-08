-module(fll_tests).
%% http://www.erlang.org/doc/apps/eunit/chapter.html
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

stack_building_test_() ->
  {foreach, spawn,
    fun start/0,
    fun stop/1,
    [
      %% fun send_to_self_and_deliver_/1
    ]
  }.


%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%

start() ->
  stack:start(),
  stack:launch_and_register_component(fll).

stop(_) ->
  ok = stack:stop().


%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%


send_to_self_and_deliver_(_) ->
  {"messages sent to own node are delivered",
    fun() ->
      Spy = nspy:mock(),
      stack:subscribe_to_events(fll, Spy),
      timer:sleep(5), % wait for messages to be processed
      fll:send(node(), hello),
      timer:sleep(5), % wait for messages to be processed
      nspy:assert_message_received(Spy, {fll, indication, {msg, hello, node()}})
    end
  }.


%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
