-module(stack_tests).
%% http://www.erlang.org/doc/apps/eunit/chapter.html
-include_lib("eunit/include/eunit.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

stack_building_test_() ->
  { "building a stack and registering modules",
    {foreach,
      fun start/0,
      fun stop/1,
      [ fun stack_begin_empty_/1,
        fun register_fll_base_module_/1,
        fun dependency_launching/1
      ]
    }
  }.


stop_stack_test_() ->
{ "stopping the stack stops launched modules",
  {setup, spawn, fun start/0, fun stop_stack_clears_bindings_/1}
}.

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%

start() ->
  stack:start().

stop(_) ->
  ok = stack:stop().


%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

stack_begin_empty_(_) ->
  {"new stack has no components",
    ?_assertEqual(sets:new(), stack:components())
  }.

register_fll_base_module_(_) ->
  { "added component is registered",
  fun () ->
    stack:launch_and_register_component(fll),
    timer:sleep(10), % wait for messages to be processed
    ?assert(sets:is_element(fll, stack:components())),
    ?assertNotEqual(whereis(fll), undefined)
  end
  }.

dependency_launching(_) ->
  { "component dependencies are launched",
    fun() ->
      stack:launch_and_register_component(sl),
      timer:sleep(10), % wait for messages to be processed
      ?assert(sets:is_element(fll, stack:components())),
      ?assertNotEqual(whereis(fll), undefined)
    end
  }.

stop_stack_clears_bindings_(_) ->
  fun() ->
    stack:launch_and_register_component(sl),
    timer:sleep(10), % wait for messages to be processed
    ?assertNotEqual(whereis(sl), undefined),
    ?assertNotEqual(whereis(fll), undefined),
    stack:stop(),
    ?assertEqual(whereis(sl), undefined),
    ?assertEqual(whereis(fll), undefined)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
