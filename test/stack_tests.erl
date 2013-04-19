-module(stack_tests).
%% http://www.erlang.org/doc/apps/eunit/chapter.html
-include_lib("eunit/include/eunit.hrl").
-include_lib("stack_state.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

integration_test_() ->
  { "integration test, building, using, tearing down a stack",
    {foreach,
      fun start/0,
      fun stop/1,
      [ fun stack_begin_empty_/1,
        fun register_fll_base_module_/1,
        fun dependency_launching/1
      ]
    }
  }.


%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%

start() ->
  spawn(fun() -> stack:start_link() end).

stop(_) ->
  ok.% = stack:stop().


%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

stack_begin_empty_(_) ->
  {"new stack has no components",
    ?_assertEqual(?STACKSET:new(), stack:query_components())
  }.

register_fll_base_module_(_) ->
  { "added component is registered",
  fun () ->
    stack:add_component(fll),
    ?assert(?STACKSET:is_element(fll, stack:query_components())),
    ?assertNotEqual(whereis(fll), undefined)
  end
  }.

dependency_launching(_) ->
  { "component dependencies are launched",
    fun() ->
      stack:add_component(sl),
      %% ?assert(?STACKSET:is_element(fll, stack:query_components())),
      ?assertNotEqual(whereis(fll), undefined)
    end
  }.

stop_stack_clears_bindings_test_() ->
  fun() ->
    stack:start_link(),
    stack:add_component(sl),
    ?assertNotEqual(undefined, whereis(sl)),
    ?assertNotEqual(undefined, whereis(fll)),
    stack:stop(),
    ?assertEqual(undefined, whereis(sl)),
    ?assertEqual(undefined, whereis(fll))
  end.




%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
