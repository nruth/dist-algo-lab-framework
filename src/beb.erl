-module(beb).
-behaviour(comp_behav).

-export([ uses/0, upon_event/2]).

uses() -> [pl].

upon_event({beb, broadcast, Msg}, State) ->
  %% io:format("beb sending: ~w~n", [Msg]),
  lists:map(fun(DestinationNodeQ) ->
    stack:trigger({pl, send, DestinationNodeQ, {beb, Msg}})
  end, stack:nodes()),
  State;

upon_event({pl, deliver, SenderNodeP, {beb, Msg}}, State) ->
  %% io:format("beb received broadcast: ~w from ~w~n", [Msg, SenderNodeP]),
  stack:trigger({beb, deliver, SenderNodeP, Msg}),
  State;

upon_event(_Other, State) ->
  %% io:format("~w ignoring event ~w~n", [?MODULE, _Other]),
  State.
