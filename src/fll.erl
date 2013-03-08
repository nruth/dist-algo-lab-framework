% fair loss links
-module(fll).
-export([dependencies/0, start/0, stop/0, event_loop/0, send/2]).

dependencies() ->
  [].


% register process receiving requests and indications at atom/pid fll
start() ->
  register(?MODULE, spawn(?MODULE, event_loop, [])).

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
      {?MODULE, Q} ! {transmission, {from, node()}, M},
      event_loop();
    {transmission, {from, Sender}, M} ->
      io:format("indication: ~w from ~w~n", [M, Sender]),
      event_loop();
    {stop, ReplyTo} ->
      % halt and unregister self
      true = unregister(?MODULE),
      ReplyTo ! {?MODULE, stopped}
  end.


send(Destination, Msg) ->
  ?MODULE ! {request, ?MODULE, {send, Destination, Msg}}.
