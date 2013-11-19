% stubborn link
-module(sl).
-behaviour(comp_behav).

-export([ uses/0, upon_event/2, start_link/0, stop/0 ]).
-record(state, {sent}).

-define(RESEND_PERIOD, 2000).

uses() -> [fll].

start_link() ->
  component:start_link(?MODULE).

stop() ->
  component:stop(?MODULE).

send(Msg, Destination) ->
  stack:trigger({fll, send, Destination, {sl,Msg}}).

upon_event(init, _) ->
  component:start_timer(?RESEND_PERIOD),
  #state{sent=sets:new()};

upon_event(timeout, State) ->
  % re-send each sent message to fll
  sets:fold(
    fun ({Q, M}, _Accum) -> send(M, Q) end,
    nil,
    State#state.sent
  ),
  component:start_timer(?RESEND_PERIOD),
  State;

upon_event({sl, send, DestinationNodeQ, Msg}, State) ->
  % Tag sl messages
  % ---------------
  % pack msg for recognition of sl msgs delivered by fll, in order to filter
  % out fll delivered msgs sent by a component other than sl

  % tell fll to send the message, and add it to the sent list for retransmission
  send(Msg, DestinationNodeQ),
  State#state{sent = sets:add_element({DestinationNodeQ, Msg}, State#state.sent)};

upon_event({fll, deliver, SenderNodeP, {sl, Msg}}, State) ->
  % indicate to the stack that sl has delivered the message
  stack:trigger({sl, deliver, SenderNodeP, Msg}),
  State;

upon_event({fll, deliver, AckSender, {sl, ack, Msg}}, State) ->
  %% io:format("sl ACK ~w from ~w~n", [Msg, AckSender]),
  %% io:format("sl SENT ~w then ~w~n", [State#state.sent, sets:del_element({node(), {sl, Msg}}, State#state.sent)]),
  State#state{sent = sets:del_element({AckSender, Msg}, State#state.sent)};

% base case, for events this module is not interested in
upon_event(_Other, State) ->
  %% io:format("~w ignoring event ~w~n", [?MODULE, Other]),
  State.

