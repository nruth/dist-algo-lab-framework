# bundling erlang code as applications

OTP defines the usual directory structure, best to just use this

how/why basis: http://learnyousomeerlang.com/designing-a-concurrent-application

OTP template generator / build tool: https://github.com/basho/rebar/wiki/Getting-started

# running rebar compiled code in the erl shell

  cd(ebin).

so that the compiled modules are accessible. You don't need to compile with
"c" inside erl, but will want to restart erl after recompiling to make sure
you have the latest beam files loaded (Erlang provides methods for updating
code in a running system, but it's out-of-scope here).


# run stack and test sl:

open 2 terminals;

terminal 1:

  cd to this directory
  ./rebar compile # should be no errors
  erl -sname foo
  cd(ebin).
  stack:start_link().
  stack:add_component(sl).
  node().

terminal 2:

  cd to this directory
  erl -sname bar
  cd(ebin).
  stack:start_link().
  stack:add_component(sl).
  stack:trigger({sl, send, PUT_TERMINAL1_NODE_RESULT_HERE, knock_knock}).




# run stack and test sl:

open 2 terminals;

terminal 1:

  cd to this directory
  ./rebar compile # should be no errors
  erl -sname foo
  cd(ebin).
  stack:start_link().
  stack:add_component(sl).
  node().

terminal 2:

  cd to this directory
  erl -sname bar
  cd(ebin).
  stack:start_link().
  stack:add_component(sl).
  stack:trigger({sl, send, PUT_TERMINAL1_NODE_RESULT_HERE, knock_knock}).




# run stack and test pl:

open 2 terminals;

terminal 1:

  cd to this directory
  ./rebar compile # should be no errors
  erl -sname foo
  cd(ebin).
  stack:start_link().
  stack:add_component(pl).
  node().

terminal 2:

  cd to this directory
  erl -sname bar
  cd(ebin).
  stack:start_link().
  stack:add_component(pl).
  stack:trigger({pl, send, 'bar@ntr-macbook-pro', knock_knock}).



# TODO

- write API instructions with code samples, enough to write p and pl
- configure nodes for p and broadcast algos


# run robots

cd(ebin). stack:start_link(). stack:add_component(dancing_robots).
stack:trigger({dancerobot, turn_left}).
stack:trigger({dancerobot, turn_right}).
stack:trigger({dancerobot, step_forward}).
stack:trigger({dancerobot, step_back}).