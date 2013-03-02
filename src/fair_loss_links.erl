-module(fair_loss_links).
-export([start/0, event_loop/0, send/2]).

% register process receiving requests and indications at atom/pid fll
start() ->
  EventLoop = spawn(fair_loss_links, event_loop, []),
  register(fll, EventLoop).


% handle receipt of send requests and received remote msgs
event_loop() ->
  receive
    {request, fll, {send, Q, M}} ->
      {fll, Q} ! {transmission, {from, node()}, M},
      event_loop();
    {transmission, {from, Sender}, M} ->
      io:format("indication: ~w from ~w~n", [M, Sender]),
      event_loop()
  end.


send(Destination, Msg) ->
  fll ! {request, fll, {send, Destination, Msg}}.
