-module(component).
-behaviour(gen_server).

-export([
start_link/2, stop/1, start_timer/1, start_timer/2,
init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2
]).

-record(state, {
  component,
  instance,
  component_state=nil
}).


init([Component, Instance]) ->
  % call arity 1 or 2 init handler, use the one that returns a state change
  InitState = case Component:upon_event({init, Instance}, nostate) of
    nostate ->
      Component:upon_event(init, nostate);
    State ->
      State
  end,
  {ok,
    #state{ component=Component, instance=Instance, component_state=InitState}
  }.


start_link(Component, Instance) ->
  %% http://erldocs.com/R15B/stdlib/gen_server.html#start_link/4
  gen_server:start_link({local, Instance}, ?MODULE, [Component, Instance], []).


stop(Component) ->
  gen_server:call(Component, stop).

% sends a timeout event to the calling pid every Duration miliseconds
start_timer(Duration) ->
  start_timer(Duration, timeout).

% send caller an event after Duration miliseconds
start_timer(Duration, TimeoutEvent) ->
  {ok, _TRef} = timer:apply_after(Duration,
    stack, trigger_one_receiver, [self(), TimeoutEvent]
  ).


handle_info({event, Event}, State) ->
  % (dynamically) call the component's event handler
  NewComponentState = (State#state.component):upon_event(Event, State#state.component_state),
  {noreply, State#state{component_state=NewComponentState}}.


handle_cast(Request, State) ->
  {stop, {unexpected_message, Request}, State}.


handle_call(stop, _From, State) ->
  {stop, normal, ok, State};
handle_call(Request, From, State) ->
  {stop, {unexpected_message, Request, From}, State}.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


terminate(normal, _State) ->
  ok.
