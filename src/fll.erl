% fair loss link
% injects random delays and losses into transmission
-module(fll).

-export([ uses/0, upon_event/2, start_link/0, stop/0 ]).

% upper bound of uniformly sampled delay range (from 0 to MAX_DELAY)
-define(MAX_DELAY, 2500).

-record(state, {reliable = true}).

uses() ->
  [].


start_link() ->
  component:start_link(?MODULE).

stop() ->
  component:stop(?MODULE).

upon_event(init, _State) ->
  %% Sets a seed for random number generation for the life of the process.
  % generate 12 crypto-safe random bytes, cast to 3 ints, use as seed
  <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
  random:seed({A,B,C}),
  #state{};

%% upon_event({fll, deliver, SenderNodeQ, Msg}, State) ->
  %% io:format("fll received message: ~w from ~w~n", [Msg, SenderNodeQ]),
  %% State;

% send without losses or delays
upon_event({fll, send, DestinationNodeQ, Msg}, State=#state{reliable = true}) ->
  stack:transmit(DestinationNodeQ, Msg),
  State;

% send with random losses or delays
upon_event({fll, send, DestinationNodeQ, Msg}, State=#state{reliable = false}) ->
  % drop some messages, delay the rest
  case random:uniform(10) of
    1 ->
      dropped_msg;
      %% io:format("fll dropping message~n");
    _ ->
      % send with delay
      %% io:format("fll delaying message~n"),
      timer:apply_after(
        random:uniform(?MAX_DELAY),
        stack, transmit, [DestinationNodeQ, Msg]
      )
  end,
  State;

% rcv without delays
upon_event({transmission_rcv, SenderP, Msg}, State=#state{reliable = true}) ->
  stack:trigger({fll, deliver, SenderP, Msg}),
  State;

% rcv with delays
upon_event({transmission_rcv, SenderP, Msg}, State=#state{reliable = false}) ->
  % delay delivery
  timer:apply_after(
    random:uniform(?MAX_DELAY),
    stack, trigger, [{fll, deliver, SenderP, Msg}]
  ),
  State;

upon_event(_Other, State) ->
  %% io:format("~w ignoring event ~w~n", [?MODULE, Other]),
  State.
