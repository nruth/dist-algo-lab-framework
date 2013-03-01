-module(heartbeat_node).
-export([spawn/0, heartbeat_responder/0]).


spawn() ->
  spawn(fun() -> heartbeat_responder() end).

heartbeat_responder() ->
  receive
    Msg={heartbeat, ReplyTo} ->
      io:format("Received ~w~n", [Msg]),
      % why do you think this might be preferable (style) to putting the message in this module?
      failure_detector:reply_to_heartbeat(ReplyTo),
      heartbeat_responder()
  end.
