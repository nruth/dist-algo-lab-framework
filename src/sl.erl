% stubborn link
-module(sl).

-record(state, {sent}).

-export([
  init/0, start_link/0, uses/0, stop/0
]).

start_link() ->
  register(?MODULE,
    spawn_link(?MODULE, init, [])
  ).

stop() ->
  ?MODULE ! stop.

uses() ->
  [fll].

init() ->
  starttimer(500),
  State = #state{sent=sets:new()},
  loop(State).

starttimer(Delta) ->
  timer:send_interval(Delta, timeout).

loop(State) ->
  % read the first message from the postbox, or wait for it to arrive
  receive
    {event, {sl, send, Q, M}} ->
      % tell fll to send the message, and add it to the sent list for retransmission
      stack:trigger({fll, send, Q, M}),
      UpdatedSent = sets:add_element({Q, M}, State#state.sent),
      loop(State#state{sent=UpdatedSent});

    {event, {fll, deliver, P, M }} ->
      % indicate to the stack that sl has delivered the message
      stack:trigger({sl, deliver, P, M}),
      loop(State);

    stop ->
      ok;

    timeout ->
      % re-send each sent message to fll
      sets:fold(
        fun ({Q, M}, _Accum) -> stack:trigger({fll, send, Q, M}) end,
        nil,
        State#state.sent
      ),
      loop(State);

    UnknownMsg ->
      io:format("~w Ignoring message: ~w~n", [?MODULE, UnknownMsg]),
      loop(State)
  end.
