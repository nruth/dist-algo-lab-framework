% stubborn link
-module(sl).
-export([ uses/0, upon_event/2, start_link/0, stop/0 ]).
-record(state, {sent}).

-define(RESEND_PERIOD, 3000).

uses() -> [fll].

start_link() ->
  component:start_link(?MODULE).

stop() ->
  component:stop(?MODULE).


upon_event(init, _) ->
  component:start_timer(?RESEND_PERIOD),
  #state{sent=sets:new()};

upon_event(timeout, State) ->
  % re-send each sent message to fll
  sets:fold(
    fun ({Q, M}, _Accum) -> stack:trigger({fll, send, Q, M}) end,
    nil,
    State#state.sent
  ),
  State;

upon_event({sl, send, DestinationNodeQ, Msg}, State) ->
  % Tag sl messages
  % ---------------
  % pack msg for recognition of sl msgs delivered by fll, in order to filter
  % out fll delivered msgs sent by a component other than sl
  PackedMsg = {sl, Msg},
  % tell fll to send the message, and add it to the sent list for retransmission
  stack:trigger({fll, send, DestinationNodeQ, PackedMsg}),
  State#state{sent = sets:add_element({DestinationNodeQ, PackedMsg}, State#state.sent)};

upon_event({fll, deliver, SenderNodeP, {sl, Msg}}, State) ->
  % indicate to the stack that sl has delivered the message
  stack:trigger({sl, deliver, SenderNodeP, Msg}),
  State;

upon_event({sl, deliver, SenderNodeQ, Msg}, State) ->
  %% io:format("sl received message: ~w from ~w~n", [Msg, SenderNodeQ]),
  State;

% base case, for events this module is not interested in
upon_event(_Other, State) ->
  %% io:format("~w ignoring event ~w~n", [?MODULE, Other]),
  State.

