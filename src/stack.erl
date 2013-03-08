-module(stack).
-export([start/0, stop/0, init/0, launch_and_register_component/1, components/0]).

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
  listen(sets:new()).

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



%% internals

listen(LaunchedComponents) ->
  receive
    {register_component, Name} ->
      io:format('registering component ~w~n', [Name]),
      NextComponents = launch_component_and_dependencies_if_missing(Name, LaunchedComponents),
      listen(NextComponents);
    {get_components, ReplyTo} ->
      ReplyTo ! {components, LaunchedComponents},
      listen(LaunchedComponents);
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
