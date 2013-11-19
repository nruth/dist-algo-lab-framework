% perfect (reliable) link with:
% * garbage collection
% * point-to-point FIFO message ordering
-module(pl_fifo_gc).
-behaviour(comp_behav).
-export([ uses/0, upon_event/2 ]).
uses() -> [sl].

% state holds last-sent-message sequence number for outgoing messages, and a 
% holdback queue for incoming messages, for each node communicated with

-type sent_counter() :: {node(), non_neg_integer()}.
-type sent_counters() :: [sent_counter()].
-record(holdback_queue, {
  % which message number we're currently waiting for
  next_message = 1 :: non_neg_integer(), 
  % all messages queued to be delivered
  messages = orddict:new() :: [{non_neg_integer(), term()}]
}).
-type holdback_queues() :: [{node(), #holdback_queue{}}].
-record(state, {
  sent = orddict:new() :: sent_counters(), 
  holdbacks = orddict:new() :: holdback_queues()
}).


upon_event(init, _) ->
  #state{};

upon_event({pl_fifo_gc, send, DestinationNodeQ, Msg}, State) ->
  % ask sl to send the msg, but first tag it with a unique identifier
  {SeqNum, NewNodeCounts} = inc_node_counter(State#state.sent, DestinationNodeQ),
  stack:trigger({sl, send, DestinationNodeQ, {pl_fifo_gc, SeqNum, Msg}}),
  State#state{sent = NewNodeCounts};

% only match sl delivered messages which pl_fifo_gc sent
% only deliver messages next in FIFO sequence; hold-back queue for others
upon_event({sl, deliver, SenderNodeP, WrappedMsg={pl_fifo_gc, Id, Msg}}, State) ->
  % ack message to halt retransmission
  % fll sufficient; any lost ack will be re-sent when msg is next received
  stack:trigger({fll, send, SenderNodeP, {sl, ack, WrappedMsg}}),

  HoldbackQueue = sender_holdback_queue(State#state.holdbacks, SenderNodeP),
  case Id >= HoldbackQueue#holdback_queue.next_message of
    true -> 
      HoldbackQueue2 = add_msg_to_holdback(HoldbackQueue, Id, Msg),
      NewHoldback = try_deliver_and_gc(SenderNodeP, HoldbackQueue2),
      State#state{
        holdbacks = orddict:store(SenderNodeP, NewHoldback, State#state.holdbacks)
      };
    false ->
      State
  end;

upon_event(_Other, State) ->
  State.


% produce a sequence of natural numbers (starting from 1) for each node
-spec inc_node_counter(sent_counters(), node()) -> {non_neg_integer(), sent_counters()}.
inc_node_counter(NodeCounters, Node) -> 
  Seq = case orddict:find(Node, NodeCounters) of
    {ok, Value} -> Value;
    error -> 0
  end + 1,
  {Seq, orddict:store(Node, Seq, NodeCounters)}.

% return the holdback queue for this sender
% or an empty one if not found (first use)
-spec sender_holdback_queue(holdback_queues(), node()) -> #holdback_queue{}.
sender_holdback_queue(HoldbackQueues, Sender) ->
  case orddict:find(Sender, HoldbackQueues) of
    {ok, Msgs} -> 
      Msgs;
    error -> 
      #holdback_queue{}
  end.

-spec add_msg_to_holdback(#holdback_queue{}, non_neg_integer(), term()) -> #holdback_queue{}.
add_msg_to_holdback(HoldbackQueue, Id, Msg) ->
  HoldbackQueue#holdback_queue{messages = 
    orddict:store(Id, Msg, HoldbackQueue#holdback_queue.messages)
  }.

% if the holdback queue has the currently awaited msg then deliver it 
% and any after it, until the sequence breaks with a missing message
% returns new holdback queue
% side-effect is delivery of several msgs when fifo order satisfied
-spec try_deliver_and_gc(node(), #holdback_queue{}) -> #holdback_queue{}.
try_deliver_and_gc(Sender, HoldbackQueue) ->
  AwaitedMessage = HoldbackQueue#holdback_queue.next_message,
  QueuedMessages = HoldbackQueue#holdback_queue.messages,
  case orddict:find(AwaitedMessage, QueuedMessages) of
    {ok, Msg} ->
      %deliver, remove from holdback, increment next_message
      deliver(Sender, Msg),
      QueueAfterDelivery = HoldbackQueue#holdback_queue{next_message = AwaitedMessage + 1,
        messages = orddict:erase(AwaitedMessage, QueuedMessages)
      },
      %recurse until a break in the sequence happens
      try_deliver_and_gc(Sender, QueueAfterDelivery);
    error ->
      %no change
      HoldbackQueue
  end.

-spec deliver(node(), term()) -> term().
deliver(SenderNodeP, Msg) ->
  stack:trigger({pl_fifo_gc, deliver, SenderNodeP, Msg}).
