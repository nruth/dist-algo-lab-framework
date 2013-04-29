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
  AllState#state{instances = orddict:store(Instance, State, AllState#state.instances)}.

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
      io:format("~w ~w proposing to replace bottom: ~w~n", [?MODULE, Instance, V]),
      InstanceState#instance{proposal = V};
    _ ->
      InstanceState
  end,
  replace_state_instance(Instance, check_round_condition(
    check_decide(InstanceState2),
    AllState#state.detectedranks
  ), AllState);

% received broadcast msg e.g. {consensus, 1, decided, V}
upon_event({beb, deliver, PFrom, {?MODULE, Instance, decided, V}}, AllState) ->
  io:format("h consensus received ~w~n", [{beb, deliver, PFrom, {?MODULE, Instance, decided, V}}]),
  Round = rank(PFrom),
  InstanceState = get_instance_state(Instance, AllState),
  io:format("storing delivered ~w ~w~n",[Round, true]),
  InstanceState2 = InstanceState#instance{
    delivered = orddict:store(Round, true, InstanceState#instance.delivered)
  },
  io:format("IState2 ~w~n",[InstanceState2]),
  InstanceState3 = case (Round < rank(node())) and (Round > InstanceState2#instance.proposer) of
    true ->
      InstanceState2#instance{proposal = V, proposer = Round};
    _ ->
      InstanceState2
  end,
  io:format("IState3 ~w~n",[InstanceState3]),
  InstanceState4 = check_round_condition(InstanceState3, AllState#state.detectedranks),
  io:format("IState4 after round condition ~w~n",[InstanceState4]),
  InstanceState5 = check_decide(InstanceState4),
  NewAllState = replace_state_instance(Instance, InstanceState5, AllState),
  io:format("NewAllState ~w~n",[NewAllState]),
  NewAllState;


upon_event({?MODULE, Instance, decide, Decided}, State) ->
  io:format("DECIDED ~w ~w: ~w~n",[?MODULE, Instance, Decided]),
  State;

upon_event(_Other, State) ->
  %% io:format("~w ignoring event ~w~n", [?MODULE, _Other]),
  State.


rank(Node) ->
  IndexedNodes = lists:zip(stack:nodes(), lists:seq(1, length(stack:nodes()))),
  {_Val, Idx} = lists:keyfind(Node, 1, IndexedNodes),
  Idx.



check_round_condition_all_instances(AllState) ->
  CheckedInstances = orddict:map(
    fun (_Instance, InstanceState) ->
      check_round_condition(InstanceState, AllState#state.detectedranks)
    end,
    AllState#state.instances
  ),
  AllState#state{instances = CheckedInstances}.


% Move to next round if appropriate.
% Calls check-broadcast when round changes.
% returns instance state
check_round_condition(InstanceState, Detectedranks) ->
  io:format("Checking if round should increase: ~w detectedranks ~w~n", [InstanceState, Detectedranks]),
  RoundDelivered = case orddict:find(InstanceState#instance.round,
    InstanceState#instance.delivered) of
    {ok, true}  ->
      % io:format("round ~w was delivered~n", [InstanceState#instance.round]),
      true;
    _ ->
      % io:format("round ~w not delivered~n",[InstanceState#instance.round]),
      false
  end,
  case sets:is_element(InstanceState#instance.round, Detectedranks) or RoundDelivered of
    true ->
      InstanceState2 = InstanceState#instance{round = (InstanceState#instance.round) + 1},
      %% io:format("IS2 round increased, ~w~n", [InstanceState]),
      check_decide(InstanceState2);
    _ ->
      InstanceState
  end.

% returns instance state
check_decide(InstanceState) ->
  io:format("Checking if can decide: ~w~n", [InstanceState]),
  case
    ( (InstanceState#instance.round == rank(node()))
      and
      (InstanceState#instance.proposal =/= bottom)
      and
      (InstanceState#instance.broadcast == false)
    ) of
    true ->
      stack:trigger({beb, broadcast,
        {?MODULE, InstanceState#instance.instance, decided,
          InstanceState#instance.proposal
        }
      }),
      stack:trigger({?MODULE, InstanceState#instance.instance, decide,
        InstanceState#instance.proposal}),
      InstanceState#instance{broadcast=true};
    _ ->
      io:format("decided no for Round ~w, Proposal ~w, Broadcast ~w~n", [InstanceState#instance.round, InstanceState#instance.proposal, InstanceState#instance.broadcast]),
      InstanceState
  end.
