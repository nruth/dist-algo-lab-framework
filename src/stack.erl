-module(stack).
-behaviour(gen_server).
-include_lib("stack_state.hrl").

-export([
start_link/0, stop/0, boot/0, boot/1, launch_cluster_application/1, halt_cluster/0,
add_component/1, add_component/2, query_components/0, trigger/1, trigger_one_receiver/2,
nodes/0, connect/1, rank/1,
init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2
]).

%% API  ===================================================


% assumes boot node sname n1@localhost
boot() ->
  boot('n1@localhost').

% configurable boot node
boot(BootNode) ->
  application:start(stack),
  stack:connect(BootNode).


% run to launch application component on all nodes; run after all connected (by boot)
launch_cluster_application(Component) ->
  gen_server:multi_call(stacknodes, lock_nodes),
  gen_server:multi_call(?MODULE, {register_component, Component, Component}),
  gen_server:multi_call(?MODULE, {start_application, Component}).

halt_cluster() ->
  rpc:multicall(erlang:nodes(), init, stop, []),
  init:stop().

% launch the stack
start_link() ->
  %% http://erldocs.com/R15B/stdlib/gen_server.html#start_link/4
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% register a component in the stack and launch its dependencies
add_component(Name) ->
  add_component(Name, Name).
add_component(Module, Instance) ->
  gen_server:call(?MODULE, {register_component, Module, Instance}).


% notify the stack (components) of an event
trigger(Event) ->
  % ask stack to broadcast to all known components for us
  gen_server:cast(?MODULE, {trigger, Event}).

% notify a single module of an event (e.g. timeout callback to self)
trigger_one_receiver(Destination, Event) ->
  % send ourselves
  send_event(Destination, Event).

% shut down the stack and any launched components
stop() ->
  gen_server:call(?MODULE, stop).

connect(Node) ->
  % TODO: add gen-server call timeout greater than retries timeout
  gen_server:call(?MODULE, {connect, Node}).

% get all nodes (those present when launch_cluster_application called)
nodes() ->
  stacknodes:nodes().

% return the integer position of Node in stack:nodes (1..length(stack:Nodes))
rank(Node) ->
  IndexedNodes = lists:zip(stack:nodes(), lists:seq(1, length(stack:nodes()))),
  {_Val, Idx} = lists:keyfind(Node, 1, IndexedNodes),
  Idx.


% returns the currently registered components
query_components() ->
  {components, Components} = gen_server:call(?MODULE, get_components),
  Components.


%% gen_server callbacks ===================================================

% entry-point for newly spawned stack process
init(_) ->
  stacknodes:start_link(),
  fll_transmit:start_link(),
  {ok, #state{}}.

terminate(normal, State) ->
  io:format("~w terminating stack.~n", [?MODULE]),
  lists:map(fun(Component) -> Component:stop() end, ?STACKSET:to_list(State#state.components)),
  io:format("~w terminated.~n", [?MODULE]),
  ok.


%% blocking sync messages

% signal to a component that it can start its application logic
% though it should be prepared to receive delivery events after init
handle_call({start_application, Component}, _From, State) ->
  send_event(Component, start_application),
  {reply, ok, State};

handle_call({connect, Node}, _From, State) ->
  io:format("~w connecting to ~w~n",[node(), Node]),
  {reply, connect_with_retries(Node), State};

% add a component to the stack
handle_call({register_component, Module, Instance}, _From, State) ->
  NextComponents = ?STACKSET:union(
    launch_component_and_dependencies_if_missing(Module, Instance),
    State#state.components
  ),
  {reply, {registered, Instance}, State#state{components=NextComponents}};

% return active stack components
handle_call(get_components, _From, State) ->
  {reply, {components, State#state.components}, State};


% shut down the stack
handle_call(stop, _From, State) ->
  gen_server:call(stacknodes, stop),
  gen_server:call(fll_transmit, stop),
  {stop, normal, ok, State}.

connect_with_retries(Node) ->
  connect_with_retries(Node, 10).

connect_with_retries(Node, 1) ->
  net_kernel:connect_node(Node);
connect_with_retries(Node, N) when N > 1->
  io:format("Connecting to ~w. Retries left ~w~n",[Node, N]),
  case net_kernel:connect_node(Node) of
    true  ->
      true;
    _Other ->
      timer:sleep(1000),
      connect_with_retries(Node, N-1)
  end.



%% non-blocking async messages

% relay event to other components
handle_cast({trigger, Event}, State) ->
  lists:map(
    fun (Receiver) -> send_event(Receiver, Event) end,
    State#state.components
  ),
  {noreply, State};

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
  launch_component_and_dependencies_if_missing(Component, Component).
launch_component_and_dependencies_if_missing(Module, Instance) ->
  case is_component_running(Instance) of
    false ->
      io:format("launching ~w at ~w~n",[Module, Instance]),
      % launch component before recursing on dependencies to break dependency loops
      % Link components so when a component crashes the whole stack crashes
      component:start_link(Module, Instance),
      % launch missing dependencies and return launched component set
      ?STACKSET:add_element(Instance,  start_component_dependencies(Module));
    true ->
      io:format("skipping ~w already running~n",[Instance]),
      ?STACKSET:new()
  end.


% returns a (possibly empty) set of launched dependencies
start_component_dependencies(Module) ->
  ?STACKSET:union(
    lists:map( fun(Dependency) ->
        % recursion is circular-dependency safe since is_component_running relies
        % on side-effect of start_component on the component higher in the stack,
        % which is called before start_component_dependencies
        launch_component_and_dependencies_if_missing(Dependency)
      end,
      uses(Module)
    )
  ).

% returns component dependencies, a list of other components
% assumes component instance name = module name
uses(Component) ->
  Component:uses().


is_component_running(Component) ->
   undefined =/= whereis(Component).

% dispatch event to one receiver
send_event(Receiver, Event) ->
  Receiver ! {event, Event}.
