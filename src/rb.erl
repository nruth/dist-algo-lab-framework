% lazy reliable broadcast
-module(rb).
-export([ uses/0, upon_event/2, stop/0 ]).
-record(state, {correct, from}).

uses() -> [beb, p].

stop() ->
  component:stop(?MODULE).


upon_event(init, _) ->
  #state{
    correct = sets:from_list(stack:nodes()),
    from = orddict:new()
  };



upon_event({rb, broadcast, Msg}, State) ->
  stack:trigger({beb, broadcast, {rb, data, make_ref(), node(), Msg}}),
  State;

upon_event({beb, deliver, _, {rb, data, Id, Sender, Msg}}, State) ->
  case sets:is_element({Id, Msg}, from(Sender, State)) of
    false ->
      stack:trigger({rb, deliver, Sender, Msg}),
      %% io:format("rb delivered ~w from ~w~n", [Msg, Sender]),
      case sets:is_element(Sender, State#state.correct) of
        false ->
          stack:trigger({beb, broadcast, {rb, data, Id, Sender, Msg}});
        _ ->
          ok
      end,
      add_from({Id, Msg}, Sender, State);
    _ ->
      State
  end;

upon_event({p, crash, PCrashed}, State) ->
  % take over broadcasting crashed node's msgs
  sets:fold(
    fun ({Id, Msg}, _Accum) ->
      stack:trigger({beb, broadcast, {rb, data, Id, PCrashed, Msg}})
    end,
    nil,
    from(PCrashed, State)
  ),
  % update state re: crash
  State#state{correct = sets:del_element(PCrashed, State#state.correct)};

% base case, for events this module is not interested in
upon_event(_Other, State) ->
  %% io:format("~w ignoring event ~w~n", [?MODULE, Other]),
  State.

% add msg to the sender's from set, returns updated State
add_from(Msg, Sender, State) ->
  State#state{
    from = orddict:store(
      Sender,
      sets:add_element(Msg, from(Sender, State)),
      State#state.from
    )
  }.

from(Sender, State) ->
  case orddict:find(Sender, State#state.from) of
    {ok, Value} ->
      Value;
    _ ->
      sets:new()
  end.
