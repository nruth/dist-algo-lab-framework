-define(STACKDICT, orddict).
-define(STACKSET, ordsets).

-record(state, {
  components = (?STACKSET):new(),
  nodes = []
}).
