-module(stack).
-behaviour(gen_server).
-include_lib("stack_state.hrl").

-export([
start_link/0, stop/0,
add_component/1, query_components/0, trigger/1,
init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2
]).

%% API  ===================================================

% launch the stack
start_link() ->
  %% http://erldocs.com/R15B/stdlib/gen_server.html#start_link/4
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


% register a component in the stack and launch its dependencies
add_component(Name) ->
  gen_server:call(?MODULE, {register_component, Name}).


% notify the stack (components) of an event
trigger(Event) ->
  gen_server:call(?MODULE, {trigger, Event}).


% shut down the stack and any launched components
stop() ->
  gen_server:call(?MODULE, stop).


% returns the currently registered components
query_components() ->
  {components, Components} = gen_server:call(?MODULE, get_components),
  Components.




%% gen_server callbacks ===================================================

% entry-point for newly spawned stack process
init([]) ->
  {ok, #state{}}.

terminate(normal, State) ->
  io:format("~w terminating stack.~n", [?MODULE]),
  lists:map(fun(Component) -> apply(Component, stop, []) end, ?STACKSET:to_list(State#state.components)),
  io:format("~w terminated.~n", [?MODULE]),
  ok.


%% blocking sync messages
% add a component to the stack
handle_call({register_component, Name}, _From, State) ->
  NextComponents = ?STACKSET:union(
    launch_component_and_dependencies_if_missing(Name),
    State#state.components
  ),
  {reply, {registered, Name}, State#state{components=NextComponents}};

% return active stack components
handle_call(get_components, _From, State) ->
  {reply, {components, State#state.components}, State};

% relay event to other components
handle_call({trigger, Event}, _From, State) ->
  lists:map(
    fun (Receiver) -> Receiver ! {event, Event} end,
    State#state.components
  ),
  {reply, ok, State};

% shut down the stack
handle_call(stop, _From, State) ->
  {stop, normal, ok, State}.


%% non-blocking async messages
% not used internally, halt if received
handle_cast(Request, State) ->
  {stop, {unexpected_message, Request}, State}.


handle_info(Msg, State) ->
  io:format("Unexpected message: ~p~n",[Msg]),
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  %% No change planned. The function is there for the behaviour,
  %% but will not be used. Only a version on the next
  {ok, State}.



%% internals  ===================================================

% launch component (and dependencies) if not already running
% returns set of newly launched components
launch_component_and_dependencies_if_missing(Component) ->
  case is_component_running(Component) of
    false ->
      % launch component before recursing on dependencies to break dependency loops
      % Link components so when a component crashes the whole stack crashes
      apply(Component, start_link, []),
      % launch missing dependencies and return launched component set
      ?STACKSET:add_element(Component,  start_component_dependencies(Component));
    true ->
      ?STACKSET:new()
  end.


% returns a (possibly empty) set of launched dependencies
start_component_dependencies(Component) ->
  ?STACKSET:union(
    lists:map( fun(Dependency) ->
        % recursion is circular-dependency safe since is_component_running relies
        % on side-effect of start_component on the component higher in the stack,
        % which is called before start_component_dependencies
        launch_component_and_dependencies_if_missing(Dependency)
      end,
      uses(Component)
    )
  ).

% returns component dependencies, a list of other components
uses(Component) ->
  apply(Component, uses, []).


is_component_running(Component) ->
   undefined =/= whereis(Component).
