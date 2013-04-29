-module(hierarchical_consensus).

-export([ uses/0, upon_event/2, stop/0 ]).
-record(state,
  {instance, detectedranks, round, proposal, proposer, delivered, broadcast}
).

uses() -> [beb, p].

stop() ->
  component:stop(?MODULE).

upon_event({init, Instance}, _) ->
  #state{
    instance=Instance,
    detectedranks = sets:new(),
    round = 1,
    proposal = bottom,
    proposer = 0,
    delivered = orddict:new(),
    broadcast = false
  };


upon_event({p, crash, PCrashed}, State) ->
  DetectedState = State#state{
    detectedranks = sets:add_element(PCrashed, State#state.detectedranks)
  },
  check_round(DetectedState);


upon_event({Instance, propose, V}, State = #state{instance=Instance, proposal=bottom}) ->
  io:format("~w proposing: ~w~n", [State#state.instance, V]),
  check_broadcast(State#state{proposal = V});


upon_event({beb, deliver, {Instance, decided, V, PFrom}}, State = #state{instance=Instance}) ->
  R = rank(PFrom),
  DeliveredState = State#state{delivered = orddict:store(R, true, State#state.delivered)},
  ProposalState = case (R < rank(node())) and (R > State#state.proposer) of
    true ->
      check_broadcast(DeliveredState#state{proposal = V, proposer = R});
    _else ->
      DeliveredState
  end,
  check_round(ProposalState);

upon_event(_Other, State) ->
  %% io:format("~w ignoring event ~w~n", [?MODULE, _Other]),
  State.

rank(Node) ->
  IndexedNodes = lists:zip(stack:nodes(), lists:seq(1, length(stack:nodes()))),
  {_Val, Idx} = lists:keyfind(Node, 1, IndexedNodes),
  Idx.

check_round(State) ->
  RoundDelivered = case orddict:find(State#state.round, State#state.delivered) of
    {ok, true}  ->
      true;
    _ ->
      false
  end,
  RoundState = case
    sets:is_element(State#state.round, State#state.detectedranks) and RoundDelivered of 
    true ->
      State#state{round = State#state.round + 1};
    _ ->
      State
  end,
  check_broadcast(RoundState).


check_broadcast(State) ->
  case
    ( (State#state.round == rank(node()))
      and
      (State#state.proposal =/= bottom)
      and
      (State#state.broadcast == false)
    ) of
    true ->
      stack:trigger({beb, broadcast, {State#state.instance, decided, State#state.proposal}}),
      stack:trigger({State#state.instance, decide, State#state.proposal}),
      State#state{broadcast=true};
    _else ->
      State
  end.
