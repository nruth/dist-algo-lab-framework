-module(hierarchical_consensus).

-export([ uses/0, upon_event/2, stop/0, propose/2 ]).
-record(state, {
  instances,
  detectedranks
}).
-record(instance,
  {instance, round, proposal, proposer, delivered, broadcast}
).

uses() -> [beb, p].

stop() ->
  component:stop(?MODULE).


propose(Value, Instance) ->
  stack:trigger({?MODULE, Instance, propose, Value}).

% look up existing consensus instance or make a new one from default values
get_instance_state(Instance, AllState) ->
  case orddict:find(Instance, AllState#state.instances) of
    {ok, Val} ->
      Val;
    _ ->
      % create initial state for new consensus instance
      #instance{
        instance = Instance,
        round = 1,
        proposal = bottom,
        proposer = 0,
        delivered = orddict:new(),
        broadcast = false
      }
  end.

% returns all-state with the instance's entry replaced
replace_state_instance(Instance, State, AllState) ->
  AllState#state{instances = stateorddict:store(Instance, State, AllState#state.instances)}.

upon_event(init, _) ->
  #state{
    instances=orddict:new(),
    detectedranks = sets:new()
  };


upon_event({p, crash, PCrashed}, AllState) ->
  AllState2 = AllState#state{
    detectedranks = sets:add_element(
      rank(PCrashed), AllState#state.detectedranks
    )
  },
  check_round_condition_all_instances(AllState2);


upon_event({?MODULE, Instance, propose, V}, AllState) ->
  InstanceState = get_instance_state(Instance, AllState),
  InstanceState2 = case InstanceState#instance.proposal of
    bottom ->
      io:format("~w proposing: ~w~n", [Instance, V]),
      InstanceState#instance{proposal = V};
    _ ->
      InstanceState
  end,
  replace_state_instance(Instance, check_broadcast(InstanceState2), AllState);

% received broadcast msg e.g. {consensus, 1, decided, V, P}
upon_event({beb, deliver, {?MODULE, Instance, decided, V, PFrom}}, AllState) ->
  R = rank(PFrom),
  InstanceState = get_instance_state(Instance, AllState),
  InstanceState2 = InstanceState#instance{delivered = orddict:store(R, true, InstanceState#instance.delivered)},
  InstanceState3 = case (R < rank(node())) and
    (R > InstanceState2#instance.proposer) of
    true ->
      InstanceState2#instance{proposal = V, proposer = R};
    _ ->
      InstanceState2
  end,
  NewAllState = replace_state_instance(Instance, InstanceState3, AllState),
  check_instance_round_condition(Instance, NewAllState);

upon_event(_Other, State) ->
  %% io:format("~w ignoring event ~w~n", [?MODULE, _Other]),
  State.


rank(Node) ->
  IndexedNodes = lists:zip(stack:nodes(), lists:seq(1, length(stack:nodes()))),
  {_Val, Idx} = lists:keyfind(Node, 1, IndexedNodes),
  Idx.



check_round_condition_all_instances(AllState) ->
  orddict:map(
    fun (InstanceState) ->
      check_broadcast(InstanceState)
    end,
    AllState#state.instances
  ).


% returns updated AllState
check_instance_round_condition(Instance, AllState) ->
  InstanceState = get_instance_state(Instance, AllState),
  replace_state_instance(Instance, check_round_condition(InstanceState, AllState#state.detectedranks), AllState).

% Move to next round if appropriate.
% Calls check-broadcast when round changes.
% returns instance state
check_round_condition(InstanceState, Detectedranks) ->
  RoundDelivered = case orddict:find(InstanceState#instance.round,
    InstanceState#instance.delivered) of
    {ok, true}  ->
      true;
    _ ->
      false
  end,
  case sets:is_element(InstanceState#instance.round, Detectedranks) and RoundDelivered of
    true ->
      check_broadcast(InstanceState#instance{round = InstanceState#instance.round + 1});
    _ ->
      InstanceState
  end.

% returns instance state
check_broadcast(InstanceState) ->
  case
    ( (InstanceState#instance.round == rank(node()))
      and
      (InstanceState#instance.proposal =/= bottom)
      and
      (InstanceState#instance.broadcast == false)
    ) of
    true ->
      stack:trigger({beb, broadcast, {InstanceState#instance.instance,
        decided, InstanceState#instance.proposal}}),
      stack:trigger({?MODULE, InstanceState#instance.instance, decide,
        InstanceState#instance.proposal}),
      InstanceState#instance{broadcast=true};
    _ ->
      InstanceState
  end.
