From  lsinf2345-framework-b51dadab5f1fbdd2.zip

changed files:
launch.sh
src/component.erl
src/dancing_robots.erl
src/fll.erl
src/fll_transmit.erl
src/sl.erl
src/stack.erl


- component:start_timer now sends only once, you must call it again in your
timeout event handler to get more timeout events (as in the book algos)

- component:start_timer takes different event triggers as its second argument
   e.g. component:start_timer(2000, timeout_gc).

 - sl.erl updated to use the new timer, check your code for the same problem

 - Dancing robots app has been updated to use new timers. Step counting stop condition revised.


The following changes smooth out some problems with application initialisation
(nodes seen by p at init) and make it easier to start and shut down all nodes

- launching component on all connected nodes now provided by stack:launch_cluster_application(Component)
- shutting down all stack nodes now provided by stack:halt_cluster.
- added launch.sh which launches n nodes in xterms, usage: ./launch.sh 5

suggested way to run the program is now:
./rebar clean compile eunit
./launch.sh 5

and in one of the Erlang shells:
stack:launch_cluster_application(dancing_robots).

then when finished, in one of the nodes:
stack:halt_cluster().
