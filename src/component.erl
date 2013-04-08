-module(component).
-behaviour(gen_server).

-export([
start_link/1, stop/1, start_timer/1, start_timer/2,
init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2
]).

-record(state, {
  component,
  component_state=nil
}).


init([Component]) ->
  Component ! {event, init},
  {ok, #state{component=Component}}.


start_link(Component) ->
  %% http://erldocs.com/R15B/stdlib/gen_server.html#start_link/4
  gen_server:start_link({local, Component}, ?MODULE, [Component], []).


stop(Component) ->
  gen_server:call(Component, stop).

% sends a timeout event to the calling pid every Duration miliseconds
start_timer(Duration) ->
  start_timer(self(), Duration).

% send a timeout event to Destination every Duration miliseconds
start_timer(Destination, Duration) ->
  timer:apply_interval(Duration,
    stack, trigger_one_receiver, [Destination, timeout]
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
