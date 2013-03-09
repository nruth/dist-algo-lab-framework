% fair loss links
-module(sl).
-export([dependencies/0, start_link/0, stop/0, event_loop/0, send/2]).

dependencies() ->
  [fll].


% register process receiving requests and indications at atom/pid fll
start_link() ->
  register(?MODULE, spawn_link(?MODULE, event_loop, [])).


stop() ->
  ?MODULE ! {stop, self()},
  receive
    {?MODULE, stopped} ->
      ok
  after 400 ->
      erlang:error(failed_to_stop)
  end.


% handle receipt of send requests and received remote msgs
event_loop() ->
  receive
    {request, ?MODULE, {send, Q, M}} ->
      fll:send(Q, M),
      event_loop();
    {stop, ReplyTo} ->
      % halt, deregistration of process name is implicit
      ReplyTo ! {?MODULE, stopped}
  end.


send(Destination, Msg) ->
  ?MODULE ! {request, ?MODULE, {send, Destination, Msg}}.
