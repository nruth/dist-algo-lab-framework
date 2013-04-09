# Message identification

An implementation detail briefly mentioned in the book is how to identify
messages, such as in pl. Pattern-matching on the whole message initially seems
like a sensible approach, though suspicions of performance aside, it
introduces a logical problem by having the same message content sent twice
identified as the same message. This is a problem for control signals (such as
heartbeat requests) which should be sent and responded to many times. We need
a solution which identifies the same message sent by different send-message
request events to be identified as different messages.

A simple solution is to introduce unique message ids. These ids can be
produced in a pseudo-random fashion by the creating node. We should check that
the probability of a collision is sufficiently low. For Erlang, we are told
that make_ref/0 "The returned reference will re-occur after approximately 2^82
calls; therefore it is unique enough for practical purposes.". It's certainly
enough for this project.

The next question is when and where to introduce these message ids.

### Russian-doll model (ref-per-component)

An approach which has the component relying only on its own actions is to
introduce its own wrapper and message id.

For example, {?MODULE, Meta, Msg} for some Msg might result in
{fll, nil, Msg} or {pl, {id, 2034}, Msg} and so on. Another use for Meta is
suggested in the section on event model changes, which also suggests a reason
for all modules to introduce this wrapper, and makes use of the thus-far unused
first term of the tuple which holds the component name.

While not all components need to track message identity, we discuss below
a change to the event model which relies on all components adding a wrapper
so that request events can be delivered to the appropriate destination, using
the wrappers as a stack-trace.


## Flat structure

Another approach would be to use a single wrapper with a meta-data field to
hold the stack-trace and any other useful information.

Message payload is wrapped once by the sending component. A library function
provides the wrapper and unique message id. Components are able to add
meta-data to the message as required.

Optimised (single id-ref for whole stack):
  structure: {msg, Id, Payload, Meta}

  - msg created at highest involved component
  - tracks who it passes through in Trace
  - Meta is arbitrary term storage for modules to piggyback info if needed

While a single id may make it easier to monitor a message's progress through
the stack during debugging, it also introduces the possibility for components
to accidentally overwrite one another's metadata.



# Event model changes

Unfortunately the book's proposed event model only works for the examples
they have given. Once you add multiple "algorithms" to the same stack you have
messages being delivered by components they were never intended for. As a
trivial example, if a stack contains pl but for some reason you decide to
send a message directly via fl, pl will still receive the message it did not
send, violating the creation property.


Two approaches, both require thought and diverge from book.


## Components responsible for packing and filtering out own messages

Use the russian-doll message approach. When requesting a component send a
message, first tag the message with the current component name, so it can
recognise its own messages when they arrive on the other process. In order to
maintain the no-creation property this should be done by all components
receiving messages from a lower component, since they cannot be sure that
components from another algorithm are not also using that lower-level
messaging component. Recall that this is the key problem, avoiding delivery
of another local algorithm's messages, which would violate an algorithm's
no-creation safety property.

This approach will work in the simple stack abstraction, which provides only
the seen-by-all event abstraction for communication between components.
However, there are performance drawbacks. Note that the message size grows
with the number of components involved, meaning larger data crossing the
network. Although messages are small this can be a problem for systems where
many messages are sent and received, such as key-value stores. In practice you
might consider the overhead relative to the size of the actual data message,
and whether the network interface is a system hotpoint (bottleneck). A further
problem may be processing cycles used in packing and unpacking, and in
pattern-matching and discarding other components' messages.By selecting who
receives events we can achieve a similar effect and remove the overhead of
processing messages destined for other components. This is partially addressed
by the approach below, which introduces notify-one and notify-all events.


## Split events into two kinds: notify-one and notify-all events

Note the similar dichotomy between these two kinds of event and between
point-to-point messages and broadcasts.


### Notify-all (announcement)

Notify-all events are triggered by failure detectors to announce suspicion of
a node. The triggering component is informing the stack as a whole of the
event, since it does not know which nodes are interested in it, or whose
algorithms require that they see it.

All components in a process's local stack will see a notify-all event.

This is a suitable definition for the book's notion of "indication", which is
poorly explained and left semantically undefined in the book.

### Notify-one

A targeted event is sent from one component to another, and indicates
ownership or expected interest. This is used when delivering messages up the
stack from fll to sl, pl, broadcast algorithms, and so on. Identifying the
recipient allows lower-level components such as fll (also pl and others are
possible) to be reused without creating new messages in the higher levels.

Only the intended (targeted) recipient will see notify-one events.

On sending a request, the sender's identity is embedded in the request's
meta-data so that the "reply" can be sent to the correct component. In the
case of messages to other nodes, this reply will be received by the like
component of the second node. This use of "reply" is awkward, since to the
receiving component it is an unsolicited delivery, not a reply.
