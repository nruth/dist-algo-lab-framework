-module(crash_node).
-export([spawn_and_die_after/1]).

spawn_and_die_after(TTL_ms) ->
  spawn(fun() ->
          ok = timer:sleep(TTL_ms)
        end
  ).
