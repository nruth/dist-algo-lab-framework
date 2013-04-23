-module(beb).

-export([ uses/0, upon_event/2, start_link/0, stop/0 ]).

uses() -> [pl].

start_link() ->
  component:start_link(?MODULE).

stop() ->
  component:stop(?MODULE).

upon_event({beb, broadcast, Msg}, State) ->
  io:format("beb sending: ~w~n", [Msg]),
  lists:map(fun(DestinationNodeQ) ->
    stack:trigger({pl, send, DestinationNodeQ, {beb, Msg}})
  end, stack:nodes()),
  State;

upon_event({pl, deliver, SenderNodeP, {beb, Msg}}, State) ->
  io:format("beb received broadcast: ~w from ~w~n", [Msg, SenderNodeP]),
  stack:trigger({beb, deliver, SenderNodeP, Msg}),
  State;

upon_event(_Other, State) ->
  %% io:format("~w ignoring event ~w~n", [?MODULE, _Other]),
  State.
