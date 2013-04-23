-module(fll_transmit).
-behaviour(gen_server).

-export([
start_link/0, transmit/2,
init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2
]).

start_link() ->
  %% http://erldocs.com/R15B/stdlib/gen_server.html#start_link/4
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


% send a message to the registered process "stack" on a remote node
% DestinationNodeQ: node() or similar (e.g. ip address)
% Msg : erlang term
transmit(DestinationNodeQ, Msg) ->
  %% io:format('Transmitting ~w to node ~w~n', [Msg, DestinationNodeQ]),
  gen_server:cast({?MODULE, DestinationNodeQ}, {transmission, {from, node()}, Msg}).

% receive transmission from another node
handle_cast({transmission, {from, SenderP}, Msg}, State) ->
  % io:format("transmission: ~w from ~w~n", [Msg, SenderP]),
  % Assume fll is always used as the low-level point-to-point comm primitive
  stack:trigger({fll, deliver, SenderP, Msg}),
  {noreply, State}.

handle_call(stop, _From, State) ->
  {stop, normal, ok, State}.

%% NOT USED

handle_info(_,State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

init(_) ->
  {ok, nostate}.

terminate(normal, _State) ->
  ok.
