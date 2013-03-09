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
        fun dependency_launching/1,
        %% TODO: event registration and receipt
        fun sl_above_fll/1
      ]
    }
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
    ?_assertEqual(?STACKSET:new(), stack:components())
  }.

register_fll_base_module_(_) ->
  { "added component is registered",
  fun () ->
    stack:launch_and_register_component(fll),
    ?assert(?STACKSET:is_element(fll, stack:components())),
    ?assertNotEqual(whereis(fll), undefined)
  end
  }.

dependency_launching(_) ->
  { "component dependencies are launched",
    fun() ->
      stack:launch_and_register_component(sl),
      ?assert(?STACKSET:is_element(fll, stack:components())),
      ?assertNotEqual(whereis(fll), undefined)
    end
  }.

stop_stack_clears_bindings_test_() ->
  fun() ->
    stack:start(),
    stack:launch_and_register_component(sl),
    ?assertNotEqual(whereis(sl), undefined),
    ?assertNotEqual(whereis(fll), undefined),
    stack:stop(),
    ?assertEqual(whereis(sl), undefined),
    ?assertEqual(whereis(fll), undefined)
  end.

sl_above_fll(_) ->
  fun() ->
    stack:launch_and_register_component(sl),
    ?assertEqual([sl], stack:get_subscribers(fll))
  end.

subscribe_events_call_test_() ->
  State = #state{},
  {spawn,
    ?_assertEqual(
      {reply, subscribed,
        State#state{parents=?STACKDICT:store(component, [subscriber], State#state.parents)}
      },
      stack:handle_call({subscribe_events, subscriber, component}, from, State)
    )
  }.

subscribe_2events_call_test_() ->
  State = #state{},
  {spawn,
    ?_assertEqual(
      {reply, subscribed,
        State#state{parents=?STACKDICT:store(component, [a, b, c], State#state.parents)}
      },
      stack:handle_call({subscribe_events, [a, b, c], component}, from, State)
    )
  }.


%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
