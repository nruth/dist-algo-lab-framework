% fair loss link
-module(fll).
-include_lib("fll_state.hrl").
-behaviour(gen_server).

-export([
uses/0, upon_event/2, start_link/0, stop/0,
init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2
]).

%% SPECIFIC

uses() ->
  [].

upon_event({request, ?MODULE, {send, Q, M}}, State) ->
  %% ?debugFmt('~nsending ~w to ~w~n', [M, Q]),
  {?MODULE, Q} ! {transmission, {from, node()}, M},
  State.


%% GENERIC

init([]) ->
  {ok, #state{}}.

start_link() ->
  %% http://erldocs.com/R15B/stdlib/gen_server.html#start_link/4
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:call(?MODULE, stop).

handle_info({event, Event}, State) ->
  {noreply, upon_event(Event, State)};

handle_info(Msg, State) ->
  io:format("Unexpected message: ~p~n",[Msg]),
  {noreply, State}.


handle_cast({transmission, {from, Sender}, M}, State) ->
  io:format("transmission: ~w from ~w~n", [M, Sender]),
  stack:trigger({?MODULE, indication, {msg, M, Sender}}),
  {noreply, State}.

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
