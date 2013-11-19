-module(beb_p2p_fifo).
-behaviour(comp_behav).

-export([ uses/0, upon_event/2]).

uses() -> [pl_fifo_gc].

upon_event({beb_p2p_fifo, broadcast, Msg}, State) ->
  lists:map(fun(DestinationNodeQ) ->
    stack:trigger({pl_fifo_gc, send, DestinationNodeQ, {beb_p2p_fifo, Msg}})
  end, stack:nodes()),
  State;

upon_event({pl_fifo_gc, deliver, SenderNodeP, {beb_p2p_fifo, Msg}}, State) ->
  stack:trigger({beb_p2p_fifo, deliver, SenderNodeP, Msg}),
  State;

upon_event(_Other, State) ->
  State.
