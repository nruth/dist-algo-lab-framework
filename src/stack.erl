-module(stack).
-behaviour(gen_server).
-include_lib("stack_state.hrl").

-export([
start_link/0, stop/0, boot/0, boot/1, start_cluster_application/1,
add_component/1, query_components/0, trigger/1, trigger_one_receiver/2, transmit/2,
nodes/0, connect/1,
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
start_cluster_application(Component) ->
  gen_server:multi_call(?MODULE, {register_component, Component}),
  gen_server:multi_call(?MODULE, {start_application, Component}).

% launch the stack
start_link() ->
  %% http://erldocs.com/R15B/stdlib/gen_server.html#start_link/4
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% register a component in the stack and launch its dependencies
add_component(Name) ->
  gen_server:call(?MODULE, {register_component, Name}).


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
  io:format("~w connecting to ~w~n",[node(), Node]),
  true = net_kernel:connect_node(Node),
  erlang:nodes().

% Returns the set of live erlang nodes.
% Will always be a superset of the correct nodes for crash-stop and crash-noisy,
% but would need revising for fail-recover to remember all nodes (rather than remove on erlang itself discovering a crash)
nodes() ->
  [node() | erlang:nodes()].

% returns the currently registered components
query_components() ->
  {components, Components} = gen_server:call(?MODULE, get_components),
  Components.

% send a message to the registered process "stack" on a remote node
% DestinationNodeQ: node() or similar (e.g. ip address)
% Msg : erlang term
transmit(DestinationNodeQ, Msg) ->
  %% io:format('Transmitting ~w to node ~w~n', [Msg, DestinationNodeQ]),
  gen_server:cast({stack, DestinationNodeQ}, {transmission, {from, node()}, Msg}).


%% gen_server callbacks ===================================================

% entry-point for newly spawned stack process
init([]) ->
  {ok, #state{nodes = lists:sort([node()|erlang:nodes()])}}.

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

% return all nodes known at stack launch (including ones who have since crashed)
handle_call(get_nodes, _From, State) ->
  {reply, State#state.nodes, State};

% shut down the stack
handle_call(stop, _From, State) ->
  {stop, normal, ok, State}.


%% non-blocking async messages

% relay event to other components
handle_cast({trigger, Event}, State) ->
  lists:map(
    fun (Receiver) -> send_event(Receiver, Event) end,
    State#state.components
  ),
  {noreply, State};

% receive transmission from another node
handle_cast({transmission, {from, SenderP}, Msg}, State) ->
  % io:format("transmission: ~w from ~w~n", [Msg, SenderP]),
  % Assume fll is always used as the low-level point-to-point comm primitive
  send_event(fll, {transmission_rcv, SenderP, Msg}),
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
  case is_component_running(Component) of
    false ->
      % launch component before recursing on dependencies to break dependency loops
      % Link components so when a component crashes the whole stack crashes
      Component:start_link(),
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
  Component:uses().


is_component_running(Component) ->
   undefined =/= whereis(Component).

% dispatch event to one receiver
send_event(Receiver, Event) ->
  Receiver ! {event, Event}.