% fair loss links
-module(fll).
-export([dependencies/0, start_link/0, stop/0, event_loop/0, send/2]).

-ifdef(TEST). %ifdef to prevent test-code compilation into ebin
-include_lib("eunit/include/eunit.hrl").
-endif.

dependencies() ->
  [].

send(Destination, Msg) ->
  %% ?debugFmt('~nsending ~w to ~w~n', [Msg, Destination]),
  ?MODULE ! {request, ?MODULE, {send, Destination, Msg}}.


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
      %% ?debugFmt('~nsending ~w to ~w~n', [M, Q]),
      {?MODULE, Q} ! {transmission, {from, node()}, M},
      event_loop();

    {transmission, {from, Sender}, M} ->
      io:format("transmission: ~w from ~w~n", [M, Sender]),
      stack:event(?MODULE, indication, {msg, M, Sender}),
      event_loop();

    {stop, ReplyTo} ->
      % halt, deregistration of process name is implicit
      ReplyTo ! {?MODULE, stopped}
  end.
