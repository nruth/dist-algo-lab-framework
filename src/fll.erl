% fair loss link
-module(fll).

-export([ uses/0, upon_event/2, start_link/0, stop/0 ]).

uses() ->
  [].


start_link() ->
  component:start_link(?MODULE).

stop() ->
  component:stop(?MODULE).


upon_event({fll, send, DestinationNodeQ, Msg}, State) ->
  stack:transmit(DestinationNodeQ, Msg),
  State;

upon_event({fll, deliver, SenderNodeQ, Msg}, State) ->
  io:format("fll received message: ~w from ~w~n", [Msg, SenderNodeQ]),
  State;

upon_event(Other, State) ->
  io:format("~w ignoring event ~w~n", [?MODULE, Other]),
  State.
