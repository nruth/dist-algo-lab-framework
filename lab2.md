Assumes:
Erlang installed and available on path

# Framework, building code, running tests

Download the lab2 framework from icampus and unzip.

Try out the included build system (https://github.com/basho/rebar/wiki) with

  ./rebar compile eunit

This executes two commands, one to compile the .erl files found in src/ to
ebin/, and another to run the tests found inlined in src/ and in separate
modules in test/.

# running code in erl

so that the compiled modules are accessible. You don't need to compile with
"c" inside erl, but will want to restart erl after recompiling to make sure
you have the latest beam files loaded (Erlang provides methods for updating
code in a running system, but it's out-of-scope here).


    cd(ebin).
    A = heartbeat_node:spawn().
    spawn(fun() -> failure_detector:watch(A) end).
    Crash = crash_node:spawn_and_die_after(5000).
    spawn(fun() -> failure_detector:watch(Crash) end).



Lab work:

Take a look around these simple Erlang files, which will work concurrently
and distributed (thanks to location transparency of Erlang PIDs), but do not
follow the book, and likely have some problems. They will give you some
ideas about the syntax, message passing, file structure, if you don't have already.

Implement Chapter 2's Fair-loss Links, Stubborn Links, and Perfect Links.

Create modules which will use the process registry in the Erlang VM (e.g. will
register the fair-loss-links module as "fll"). Each of your VMs will register
one of these modules (as in the book).

These abstractions as in the book don't really fit in single-VM Erlang, so
you will want to use 2 or more Erlang VMs and form the link between them.

