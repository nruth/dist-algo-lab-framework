% consensus-based total order broadcast
-module(tob).

-export([ uses/0, upon_event/2, stop/0, broadcast/1 ]).
-record(state, {decided, unordered, delivered, round, wait}).

uses() -> [rb, hc].

stop() ->
  component:stop(?MODULE).

broadcast(Msg) ->
  stack:trigger({tob, broadcast, Msg}).


upon_event(init, _) ->
  #state{
    unordered= ordsets:new(),
    delivered = ordsets:new(),
    decided = orddict:new(),
    round = 1,
    wait = false
  };


upon_event({tob, broadcast, Msg}, State) ->
  stack:trigger({rb, broadcast, {tob, make_ref(), Msg}}),
  State;

upon_event({rb, deliver, PSender, {tob, Id, Msg}}, State) ->
  StateMaybeNewMsg = case ordsets:is_element({Id, Msg}, State#state.delivered) of
    true ->
      State;
    _ ->
      State#state{unordered = ordsets:add_element({PSender, Id, Msg}, State#state.unordered)}
  end,
  consensus(StateMaybeNewMsg);

%% upon_event({hc, Round, decide, Decided}, State)
%%   when State#state.round == Round ->
%%     io:format("TOB receiving consensus result ~w ~w~n", [Round, Decided]),


upon_event({hc, Round, decide, Decided}, State) ->
  % TODO: can deliver check and past decisions buffer
  % TODO: or change consensus to barrier-sync on new-round installation with live nodes so round is only ever current round, so that WAIT var actually works!
    %% io:format("TOB received ~w waiting for ~w ~n", [Round, State#state.round]),
    try_deliver(State#state{decided = orddict:store(Round, Decided, State#state.decided)});


%% upon_event({tob, deliver, PSender, Msg}, State) ->
  %% io:format("TOB DELIVER ~w from ~w~n", [Msg, PSender]),
  %% State;

% base case, for events this module is not interested in
upon_event(_Other, State) ->
  %% io:format("~w ignoring event ~w~n", [?MODULE, Other]),
  State.



try_deliver(State) ->
  case orddict:find(State#state.round, State#state.decided) of
    {ok, Decided} ->
      %% io:format("TOB Delivering ~w: ~w~n", [State#state.round, State#state.decided]),
      AfterDeliver = deliver(Decided, State),
      AfterDeliver2 = AfterDeliver#state{decided = orddict:erase(State#state.round, AfterDeliver#state.decided)},
      try_deliver(AfterDeliver2);
    _ ->
      State
  end.

deliver(Decided, State) ->
  % sort and tob deliver the set of messages agreed on for this delivery round
  DeliveryList = lists:sort(ordsets:to_list(Decided)),
  %% io:format("TOB delivering ~w~n", [DeliveryList]),
  lists:map(
    fun ({PSender, _Id, Msg}) -> stack:trigger({tob, deliver, PSender, Msg}) end,
    DeliveryList
  ),
  % see book errata regarding new value of delivered
  Msgs = lists:map(
    fun ({_PSender, _Id, Msg}) -> Msg end,
    DeliveryList
  ),
  %% io:format("Updating state~w~n", [State]),
  AfterDeliveryState = State#state{
    delivered = ordsets:union(Msgs, State#state.delivered),
    unordered = ordsets:subtract(State#state.unordered, Decided),
    round = State#state.round + 1,
    wait = false
  },
  %% io:format("Updated state~w~n", [AfterDeliveryState]),
  AfterDeliveryState.

% run consensus on unordered set if not already running
consensus(State) ->
  case (State#state.unordered =/= ordsets:new()) and (State#state.wait == false) of
    true ->
      hc:propose(State#state.unordered, State#state.round),
      State#state{wait=true};
    false ->
      State
  end.