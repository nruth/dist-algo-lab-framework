% WARNING: this message has not been tested at all
% TODO: run it against the robots demo app

% perfect (reliable) link with:
% * garbage collection
-module(pl_gc).
-behaviour(comp_behav).
-export([ uses/0, upon_event/2 ]).
uses() -> [sl].

% state holds sent and delivered message sequence counters 
% for each node communicated with

-record(state, {
  sent = orddict:new() :: sent_counters(), 
  delivered = orddict:new() :: delivered_msgs()
}).

% sent-from-here counts just need a k/v of node/sequence-number
-type sent_counter() :: {node(), non_neg_integer()}.
-type sent_counters() :: [sent_counter()].

-spec inc_node_counter(sent_counters(), node()) -> {non_neg_integer(), sent_counters()}.
inc_node_counter(NodeCounters, Node) -> 
  Seq = case orddict:find(Node, NodeCounters) of
    {ok, Value} -> Value;
    error -> 0
  end + 1,
  {Seq, orddict:store(Node, Seq, NodeCounters)}.


% tracking delivered msgs with gc takes a little extra complexity
-record(delivered_msgs_from, {
  % messages < delivered_lessthan have been garbage collected and should not be re-delivered
  delivered_lessthan :: non_neg_integer(), 
  % all messages delivered but not yet garbage collected
  messages :: set()
}).
-type delivered_msgs() :: [{node(), #delivered_msgs_from{}}].

upon_event(init, _) ->
  #state{};

upon_event({pl_gc, send, DestinationNodeQ, Msg}, State) ->
  % ask sl to send the msg, but first tag it with a unique identifier
  {SeqNum, NewNodeCounts} = inc_node_counter(State#state.sent, DestinationNodeQ),
  stack:trigger({sl, send, DestinationNodeQ, {pl_gc, SeqNum, Msg}}),
  State#state{sent = NewNodeCounts};

% only match sl delivered messages which pl_gc sent
upon_event({sl, deliver, SenderNodeP, WrappedMsg={pl_gc, Id, Msg}}, State) ->
  % ack message to halt retransmission
  % fll sufficient; any lost ack will be re-sent when msg is next received
  stack:trigger({fll, send, SenderNodeP, {sl, ack, WrappedMsg}}),

  % only deliver messages not previously delivered
  % deliver in any order
  % garbage collect up to msg n when all including n have been delivered
  SenderPreviousMsgs = sender_previous_msgs(State#state.delivered, SenderNodeP),
  NewSenderPreviousMsgs = deliver_new_and_try_gc(Id, Msg, SenderPreviousMsgs),
  State#state{
    delivered = orddict:store(SenderNodeP, NewSenderPreviousMsgs, State#state.delivered)
  };

upon_event(_Other, State) ->
  State.

deliver_new_and_try_gc(Id, _, DeliveredFrom) when (Id < DeliveredFrom#delivered_msgs_from.delivered_lessthan) -> 
  % delivered before, do nothing
  DeliveredFrom;
deliver_new_and_try_gc(Id, Msg, DeliveredFrom) 
when Id >= DeliveredFrom#delivered_msgs_from.delivered_lessthan -> 
  case sets:is_element(Id, DeliveredFrom#delivered_msgs_from.messages) of
    true ->
      % delivered before, do nothing
      DeliveredFrom;
    false ->
      % deliver and record delivery
      deliver(DeliveredFrom, Msg),
      gc(DeliveredFrom#delivered_msgs_from{
        messages = sets:add_element(Id, DeliveredFrom#delivered_msgs_from.messages) 
      })
  end.

deliver(SenderNodeP, Msg) ->
  stack:trigger({pl_gc, deliver, SenderNodeP, Msg}).

% remove uninterrupted sequences of delivered messages >= past delivery threshold int
% e.g. delievered_lessthan = 5, messages = [6, 7, 9, 10] gives
% delievered_lessthan = 8, messages = [9, 10] 
% which then waits for 8 before cleaning out the rest
gc(DeliveredFrom = #delivered_msgs_from{messages=Messages, delivered_lessthan=DeliveredLessThan }) ->
  case sets:is_element(DeliveredLessThan, Messages) of
    true ->
      gc(DeliveredFrom#delivered_msgs_from{
        messages = sets:del_element(DeliveredLessThan, Messages),
        delivered_lessthan = DeliveredLessThan + 1
        }
      );
    false ->
      DeliveredFrom
  end.
  
-spec sender_previous_msgs(delivered_msgs(), node()) -> #delivered_msgs_from{}.
sender_previous_msgs(ReceivedMsgs, Node) -> 
  case orddict:find(Node, ReceivedMsgs) of
    {ok, Msgs} -> 
      Msgs;
    error -> 
      #delivered_msgs_from{}
  end.
