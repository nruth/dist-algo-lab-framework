% "perfect" failure detector
-module(p).
-export([ uses/0, upon_event/2, stop/0 ]).
-record(state, {alive, detected}).

-define(DELTA, 1000).

uses() -> [pl].

stop() ->
  component:stop(?MODULE).


upon_event(init, _) ->
  component:start_timer(?DELTA),
  #state{
    % start with all (assumed connected by stack bootup) nodes alive
    alive = sets:from_list(stack:nodes()),
    % and no nodes failed
    detected = sets:new()
  };

% heartbeat timeout expired, time to detect new failures
upon_event(timeout, State) ->
  % functional update of detected by folding over all nodes
  DetectedUpdated = lists:foldl(
    fun (PNode, DetectedAccum) ->
      % send next heartbeat (whether crashed or not; see perf notes of book)
      stack:trigger({pl, send, PNode, heartbeat_request}),

      case (not sets:is_element(PNode, State#state.alive))
            and (not sets:is_element(PNode, State#state.detected))
      of
        true ->
          % no reply received, add to crashed
          stack:trigger({p, crash, PNode}),
          sets:add_element(PNode, DetectedAccum);
        false ->
          DetectedAccum
      end
    end,
    State#state.detected,
    stack:nodes() % this could be smarter by only checking live nodes of previous round
  ),

  component:start_timer(?DELTA),
  State#state{
    alive = sets:new(),
    detected = DetectedUpdated
  };

% hb request received
upon_event({pl, deliver, SenderNodeQ, heartbeat_request}, State) ->
  % reply to the hb request
  stack:trigger({pl, send, SenderNodeQ, heartbeat_reply}),
  State;

% hb reply received
upon_event({pl, deliver, SenderNodeP, heartbeat_reply}, State) ->
  % record the heartbeat sender in alive for this round
  State#state{alive = sets:add_element(SenderNodeP, State#state.alive)};

upon_event(_Other, State) ->
  %% io:format("~w ignoring event ~w~n", [?MODULE, _Other]),
  State.
