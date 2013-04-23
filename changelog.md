From  lsinf2345-framework-b51dadab5f1fbdd2.zip

 - component:start_timer now sends only once, you must call it again in your
timeout event handler to get more timeout events (as in the book algos)

 - sl.erl has been changed to use the new timer (timeout event makes a new timer)

 - Dancing robots app has been updated to use new timers. Stop condition revised.

 - component:start_timer now takes a replacement timeout event as its
second argument.
e.g. component:start_timer(2000, timeout_gc) might be useful as part of
running gc every 20 seconds




