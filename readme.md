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


# TODO

1. link to sets documentation

2. write instructions for creating perfect-links with enough documentation

3. write explanation of fll and sl code
