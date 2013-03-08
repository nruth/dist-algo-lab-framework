-module (nspy).
-export ([new/1, wrapper/1, mock/0, spy/2, assert_message_received/2, assert_message_not_received/2, assert_message_received_n_times/3]).
-define (NODEBUG, true).
-include_lib("eunit/include/eunit.hrl").

% API
% ===

% a test double / mock process to use as a non-replying endpoint (null object) for messages
mock() -> new([]).


% a test spy to wrap another process, monitoring the messages sent to that process and relaying them to the intended receiver
% doesn't consider node failures or message synchrony, but you probably shouldn't be relying on that anyway.
wrapper(Target) -> new([Target]).

  passthrough_wrapper_spy_test() ->
    ProcUnderTest = nspy:mock(),
    PassthroughSpy = nspy:wrapper(ProcUnderTest),
    PassthroughSpy ! hi,
    assert_message_received(PassthroughSpy, hi),
    assert_message_received(ProcUnderTest, hi).



%spawn a new spy process with no messages and a list of processes to forward messages to
new(RelayMessagesTo) ->
  spawn(nspy, spy, [[], RelayMessagesTo]).



% asks the spy to return its current messages received list
% n.b. assume asynchronous messaging and use appropriate sleep periods to allow message delivery
get_messages_from_spy(Spy) ->
  Spy ! {nspy_list_messages, self()},
  receive {nspy_messages, Messages} -> Messages end.

  get_messages_from_spy_test() ->
    Spy = spawn(nspy, spy, [[a_message], []]),
    ?assertEqual(get_messages_from_spy(Spy), [a_message]). %or timeout failure



% ASSERTS
% =======

assert_message_received(Spy, Expected) ->
  Messages = get_messages_from_spy(Spy),
  io:format("~n[SPY] expected ~p received ~p~n", [Expected, Messages]),
  ?assert(lists:member(Expected, Messages)).

  assert_message_received_success_test() ->
    Spy = nspy:mock(),
    Spy ! hi,
    Spy ! ho,
    timer:sleep(200),
    assert_message_received(Spy, hi),
    assert_message_received(Spy, ho).

  assert_message_received_failure_test() ->
    Spy = nspy:mock(),
    Spy ! hi,
    timer:sleep(200),
    ?assertError({assertion_failed, _}, assert_message_received(Spy, ho)).



assert_message_not_received(Spy, Message) ->
  assert_message_received_n_times(Spy, Message, 0).

  assert_message_not_received_success_test() -> assert_message_not_received(nspy:mock(), hi).
  assert_message_not_received_failure_test() ->
    Spy = nspy:mock(),
    Spy ! hi,
    ?assertError({assertEqual_failed, _}, assert_message_not_received(Spy, hi)).



assert_message_received_n_times(Spy, Expected, NTimes) ->
  Messages = get_messages_from_spy(Spy),
  FilteredMessages = [M || M <- Messages, M == Expected],
  io:format("~n[SPY] expected ~p ~p times in received messages:~p,~nfound matches: ~p ~n", [Expected, NTimes, Messages, FilteredMessages]),
  ?debugFmt("Filtering messages matching ~p from ~p, found ~p~n",[Expected, Messages, FilteredMessages]),
  ?assertEqual(NTimes, length(FilteredMessages)).

  assert_message_received_n_times_failure_test() ->
    Spy = nspy:mock(),
    ?assertError({assertEqual_failed, _}, assert_message_received_n_times(Spy, hi, 1)).

  assert_message_received_n_times_success_test() ->
    Spy = nspy:mock(),
    Spy ! hi,
    assert_message_received_n_times(Spy, hi, 1),
    Spy ! hi,
    assert_message_received_n_times(Spy, hi, 2).

  assert_message_received_n_times_ignores_other_messages_test() ->
    Spy = nspy:mock(),
    Spy ! hi,
    assert_message_received_n_times(Spy, hi, 1),
    assert_message_received_n_times(Spy, ho, 0),
    Spy ! ho,
    assert_message_received_n_times(Spy, ho, 1),
    assert_message_received_n_times(Spy, hi, 1).



% CORE
% ====

spy (Messages, RelayMessagesTo) ->
  receive
    {nspy_list_messages, ReplyTo}  ->
      ReplyTo ! {nspy_messages, Messages},
      ?debugFmt("Node ~p requested received messages, sending: ~p~n", [ReplyTo, Messages]),
      spy(Messages, RelayMessagesTo);
    Message ->
      ?debugFmt("Spy ~p received message: ~p~n", [self(), Message]),
      BroadcastMessage = fun(Receiver) -> Receiver ! Message end,
      lists:map(BroadcastMessage, RelayMessagesTo),
      spy([Message | Messages], RelayMessagesTo)
  end.