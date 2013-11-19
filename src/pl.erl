% perfect (reliable) link
-module(pl).
-behaviour(comp_behav).

-export([ uses/0, upon_event/2 ]).
-record(state, {delivered}).

uses() -> [sl].

upon_event(init, _) ->
  #state{delivered=sets:new()};

upon_event({pl, send, DestinationNodeQ, Msg}, State) ->
  %% io:format("pl sending message: ~w to ~w~n", [Msg, DestinationNodeQ]),
  % ask sl to send the msg, but first tag it with a unique identifier
  stack:trigger({sl, send, DestinationNodeQ, {pl, make_ref(), Msg}}),
  State;

% only match sl delivered messages which pl sent
upon_event({sl, deliver, SenderNodeP, WrappedMsg={pl, Id, Msg}}, State) ->
  % acknowledg message to halt retransmission
  stack:trigger({fll, send, SenderNodeP, {sl, ack, WrappedMsg}}),
  % only deliver messages not previously delivered
  State#state{
    delivered =  case sets:is_element(Id, State#state.delivered) of
                    true ->
                      State#state.delivered;
                    false ->
                      stack:trigger({pl, deliver, SenderNodeP, Msg}),
                      sets:add_element(Id, State#state.delivered)
                  end
  };

%% upon_event({pl, deliver, SenderNodeQ, Msg}, State) ->
%%   io:format("pl received message: ~w from ~w~n", [Msg, SenderNodeQ]),
%%   State;

upon_event(_Other, State) ->
  %% io:format("~w ignoring event ~w~n", [?MODULE, _Other]),
  State.

