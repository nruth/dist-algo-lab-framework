-module(failure_detector).
-export([watch/1, reply_to_heartbeat/1]).

watch(Pid) ->
  Pid ! {heartbeat, self()},
  receive
    {hb_reply, Pid} ->
      io:format("hb received from ~w~n", [Pid]),
      timer:sleep(3000),
      watch(Pid)
  after
    2000 ->
      io:format("Failure suspected, node: ~w~n", [Pid]),
      {suspected, Pid}
  end.


reply_to_heartbeat(ReplyTo) ->
  %% note that self() is the executing process PID, not a reference to this
  %% module; the process is likely running a function from another module, which
  %% calls this as it might a library function
  ReplyTo ! {hb_reply, self()}.
