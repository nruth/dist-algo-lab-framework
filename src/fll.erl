% fair loss link
% injects random delays and losses into transmission
-module(fll).

-export([ uses/0, upon_event/2, start_link/0, stop/0 ]).

uses() ->
  [].


start_link() ->
  component:start_link(?MODULE).

stop() ->
  component:stop(?MODULE).

upon_event(init, State) ->
  %% Sets a seed for random number generation for the life of the process.
  % generate 12 crypto-safe random bytes, cast to 3 ints, use as seed
  <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
  random:seed({A,B,C}),
  State;

upon_event({fll, deliver, SenderNodeQ, Msg}, State) ->
  io:format("fll received message: ~w from ~w~n", [Msg, SenderNodeQ]),
  State;

upon_event({fll, send, DestinationNodeQ, Msg}, State) ->
  case random:uniform(10) of
    1 ->
      io:format("fll dropping message~n");
    2 ->
      % send with delay
      timer:apply_after(random:uniform(500), stack, transmit, [DestinationNodeQ, Msg]);
    _ ->
      stack:transmit(DestinationNodeQ, Msg)
  end,
  State;

upon_event(Other, State) ->
  %% io:format("~w ignoring event ~w~n", [?MODULE, Other]),
  State.
