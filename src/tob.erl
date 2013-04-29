% consensus-based total order broadcast
-module(tob).

-export([ uses/0, upon_event/2, stop/0 ]).
-record(state, {unordered, delivered, round, wait}).

uses() -> [rb, hierarchical_consensus].

stop() ->
  component:stop(?MODULE).

% run consensus on unordered set if not already running
consensus(State) ->
  case (State#state.unordered =/= sets:new()) and (State#state.wait == false) of
    true ->
      heirarchical_consensus:propose(State#state.unordered, State#state.round),
      State#state{wait=true};
    false ->
      State
  end.

upon_event(init, _) ->
  #state{
    unordered=sets:new(),
    delivered = sets:new(),
    round = 1,
    wait = false
  };


upon_event({tob, broadcast, Msg}, _State) ->
  stack:trigger({rb, broadcast, Msg});

upon_event({rb, deliver, PSender, Msg}, State) ->
  StateMaybeNewMsg = case sets:is_element(Msg, State#state.delivered) of
    true ->
      State;
    false ->
      State#state{unordered = sets:add_element({PSender, Msg}, State#state.unordered)}
  end,
  consensus(StateMaybeNewMsg);

upon_event({hierarchical_consensus, Round, decide, Decided}, State)
  when State#state.round == Round ->
    % sort and tob deliver the set of messages agreed on for this delivery round
    lists:foreach(
      fun ({PSender, Msg}) -> stack:trigger({tob, deliver, PSender, Msg}) end,
      lists:sort(sets:to_list(Decided))
    ),
    % see book errata regarding new value of delivered
    Msgs = sets:map(fun ({_PSender, Msg}) -> Msg end, Decided),
    State#state{
      delivered = sets:union(Msgs, State#state.delivered),
      unordered = sets:remove(Decided, State#state.unordered),
      round = State#state.round + 1,
      wait = false
    };

% base case, for events this module is not interested in
upon_event(_Other, State) ->
  %% io:format("~w ignoring event ~w~n", [?MODULE, Other]),
  State.

