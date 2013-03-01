-module(crash_node).
-export([spawn_and_die_after/1]).

% non-blocking; creates a node and kills it after TTL_ms miliseconds
% returns the temporary node's pid as {ok, NodePid}
spawn_and_die_after(TTL_ms) ->
  % spawn the node
  Node = heartbeat_node:spawn(),

  % spawn another process to kill the node later (rather than block the executing process)
  spawn(fun() ->
    % kill the node after its TTL expires
    ok = timer:sleep(TTL_ms),
    timer:kill_after(TTL_ms, Node)
  end),
  Node.
