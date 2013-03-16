-module(component).
-behaviour(gen_server).

-export([
start_link/1, stop/1,
init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2
]).

-record(state, {
  component
}).


init([Component]) ->
  {ok, #state{component=Component}}.


start_link(Component) ->
  %% http://erldocs.com/R15B/stdlib/gen_server.html#start_link/4
  gen_server:start_link({local, Component}, ?MODULE, [Component], []).


stop(Component) ->
  gen_server:call(Component, stop).


handle_info({event, Event}, State) ->
  % (dynamically) call the component's event handler
  NewState = apply(State#state.component, upon_event, [Event, State]),
  {noreply, NewState};
handle_info(Msg, State) ->
  io:format("Unexpected message: ~p~n",[Msg]),
  {noreply, State}.


handle_cast(Request, State) ->
  {stop, {unexpected_message, Request}, State}.


handle_call(stop, _From, State) ->
  {stop, normal, ok, State};
handle_call(Request, From, State) ->
  {stop, {unexpected_message, Request, From}, State}.


code_change(_OldVsn, State, _Extra) ->
  %% No change planned. The function is there for the behaviour,
  %% but will not be used. Only a version on the next
  {ok, State}.


terminate(normal, _State) ->
  ok.
