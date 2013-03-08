-module(stack).
-export([start/0, stop/0, init/0, launch_and_register_component/1,
components/0, get_subscribers/1, subscribe_to_events/2, event/3
]).

-ifdef(TEST). %ifdef to prevent test-code compilation into ebin
-include_lib("eunit/include/eunit.hrl").
-endif.

% launch and bind the stack module
start() ->
  Stack = spawn(?MODULE, init, []),
  register(?MODULE, Stack),
  ?MODULE.

stop() ->
  ?MODULE ! {stop, self()},
  receive
    {?MODULE, stopped} ->
      ok
  after 500 ->
      erlang:error(failed_to_stop)
  end.

% entry-point for newly spawned stack process
init() ->
  listen(sets:new(), dict:new()).

% register a component in the stack and launch its dependencies
launch_and_register_component(Name) ->
  ?MODULE ! {register_component, Name}.

% returns the currently registered components data
components() ->
  ?MODULE ! {get_components, self()},
  receive
    {components, Components} ->
      Components
  end.

get_subscribers(Component) ->
  ?MODULE ! {get_subscribers, self(), Component},
  receive
    {subscribers, Components} ->
      Components
  end.


event(Module, indication, Event) ->
  relay_event({Module, indication, Event}, get_subscribers(Module));
event(Module, request, Event) ->
  relay_event({Module, request, Event}, get_subscribers(Module)).

subscribe_to_events(Component, Subscriber) ->
  ?MODULE ! {subscribe_events, Subscriber, Component, self()},
  receive
    {subscribed, ok} ->
      ok
  end.

%% internals

listen(LaunchedComponents, Parents) ->
  receive
    {register_component, Name} ->
      NextComponents = launch_component_and_dependencies_if_missing(Name, LaunchedComponents),
      NextParents = add_subscribers(Name, dependencies(Name), Parents),
      listen(NextComponents, NextParents);

    {subscribe_events, Subscriber, Component, AckTo} ->
      NextParents = add_subscribers(Component, [Subscriber], Parents),
      AckTo ! {subscribed, ok},
      listen(LaunchedComponents, NextParents);

    {get_components, ReplyTo} ->
      ReplyTo ! {components, LaunchedComponents},
      listen(LaunchedComponents, Parents);

    {get_subscribers, ReplyTo, Component} ->
      ReplyTo ! {subscribers, dict:fetch(Component, Parents)},
      listen(LaunchedComponents, Parents);

    {stop, ReplyTo} ->
      % halt all launched components, then halt and unregister self
      lists:map(fun(Component) -> apply(Component, stop, []) end, sets:to_list(LaunchedComponents)),
      true = unregister(?MODULE),
      ReplyTo ! {?MODULE, stopped}
  end.

% launch component (and check dependencies) if not already running
% returns updated launched component list
launch_component_and_dependencies_if_missing(Component, LaunchedComponents) ->
  case is_component_running(Component, LaunchedComponents) of
    false ->
      lists:foldl(
        fun
          (Dependency, Accum) ->
            ok = launch_and_bind_component_if_not_runnning(Dependency, Accum),
            sets:add_element(Dependency, Accum)
        end,
        LaunchedComponents,
        [Component | dependencies(Component)]
      );

    true ->
      % already launched, do nothing
      LaunchedComponents
  end.

add_subscribers(Component, Children, Parents) ->
  %% ?debugFmt('subscribing ~w to ~w with prior ~w', [Children, Component, Parents]),
  lists:foldl(
    fun(Child, Accum) ->
      dict:append(Child, Component, Accum)
    end, Parents, Children
  ).


-ifdef(TEST). %ifdef to prevent test-code compilation into ebin




add_subscribers_test() ->
  Actual = dict:to_list(add_subscribers(p, [b, c], dict:new())),
  Expected = dict:to_list(dict:store(c, [p], (dict:store(b, [p], dict:new())))),
  ?assertEqual(Expected, Actual).

add_multiple_subscribers_test() ->
  Actual = dict:to_list(add_subscribers(q, [b, c], add_subscribers(p, [b, c], dict:new()))),
  Expected = dict:to_list(dict:store(c, [p, q], (dict:store(b, [p, q], dict:new())))),
  ?assertEqual(Expected, Actual).

-endif.



dependencies(Component) ->
  apply(Component, dependencies, []).

% call component's start&bind function
launch_and_bind_component_if_not_runnning(Name, LaunchedComponents) ->
  case is_component_running(Name, LaunchedComponents) of
    false ->
      apply(Name, start, []),
      ok;
    true ->
      ok
  end.

is_component_running(Component, LaunchedComponents) ->
  sets:is_element(Component, LaunchedComponents).


relay_event(Event, Receivers) ->
  %% ?debugFmt('relaying ~w to ~w~n', [Event, Receivers]),
  lists:map(fun
    (Receiver) ->
      Receiver ! Event
  end, Receivers).


-ifdef(TEST). %ifdef to prevent test-code compilation into ebin

relay_event_test() ->
  Spy1 = nspy:mock(),
  Spy2 = nspy:mock(),
  relay_event(arbitrary_terms, [Spy1, Spy2]),
  nspy:assert_message_received(Spy1, arbitrary_terms),
  nspy:assert_message_received(Spy2, arbitrary_terms).

-endif.
