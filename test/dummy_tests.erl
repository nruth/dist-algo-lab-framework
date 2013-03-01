%% If you want to separate your test code from your normal code (at least for
%% testing the exported functions), you can simply write the test functions in
%% a module named m_tests (note: not m_test), if your module is named m. Then,
%% whenever you ask EUnit to test the module m, it will also look for the
%% module m_tests and run those tests as well.
-module(dummy_tests).

%% http://www.erlang.org/doc/apps/eunit/chapter.html
-include_lib("eunit/include/eunit.hrl").

%% some trivial example tests

one_plus_one_test() ->
  2 = 1 + 1.

true_is_true_test() ->
  ?assert(true).

symbol_equality_test() ->
  ?assertEqual(foo, foo),
  ?assertNotEqual(foo, bar).

pattern_matching_test() ->
  ?assertMatch({_, fixed, 3 }, {ignored, fixed, 1+2}).
