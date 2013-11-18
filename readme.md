# Erlang Component Framework

This project aims to be a faithful implementation of the component model
presented in *C. Cachin, R. Guerraoui, and L. Rodrigues, Introduction to
Reliable and Secure Distributed Programming. Springer-Verlag, Berlin 2nd
Edition, 2011*. That said, this software is unfinished, and for more
developed stacks we suggest [Appia](http://appia.di.fc.ul.pt/) or
[Kompics](kompics.sics.se).


Modules and applications are implemented as callback functions on an event model,
where each component waits for messages published by components below it in the
stack, its own state, and the messages it publishes for components or 
applications higher in the stack.

This approach eases building and reasoning about algorithms such as consensus 
or total order broadcast by making use of simpler abstractions such
as perfect links and point-to-point orderings, and making explicit the failure
model assumed and detectors used.

The following are residual notes not yet cleaned up, and may not be useful
documentation. The files
projects/dancing-robot-project/lab3-implementing-basic-abstractions.pdf and
projects/dancing-robot-project/assignment.pdf may be more useful.

# bundling erlang code as applications

OTP defines the usual directory structure, best to just use this

how/why basis: http://learnyousomeerlang.com/designing-a-concurrent-application

OTP template generator / build tool: https://github.com/basho/rebar/wiki/Getting-started

# compiling debug info enabled code (for the debugger) with rebar

set in rebar.config:

    {erl_opts, [debug_info]}.



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



# run robots

    cp projects/dancing-robot-project/dancing_robots.erl src/
    ./rebar compile # should be no errors

See robots assignment.pdf for launch instructions
