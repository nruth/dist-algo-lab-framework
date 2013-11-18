-module(tob_playout_buffer).

-export([ uses/0, upon_event/2, stop/0 ]).

-define(PLAYBACK_PERIOD, 1000).
-define(PREBUFFER, 15).

-record(state, {
  queue = queue:new(), playing = false
}).

uses() -> [tob].

stop() ->
  component:stop(?MODULE).

upon_event(init, _) ->
  component:start_timer(?PLAYBACK_PERIOD),
  #state{};

upon_event({tob, deliver, PSender, Msg}, State) ->
  %% io:format("buffering step~n"),
  State#state{queue = queue:in({PSender, Msg}, State#state.queue)};

upon_event(timeout, State) ->
  component:start_timer(?PLAYBACK_PERIOD),
  case State#state.playing or (queue:len(State#state.queue) > ?PREBUFFER) of
    true ->
      case queue:out(State#state.queue) of
        {{value, {PSender, Msg}}, Q2} ->
          %% io:format("playing step~n"),
          stack:trigger({?MODULE, deliver, PSender, Msg}),
          State#state{queue = Q2};
        _ ->
          %% io:format("no step buffered~n"),
          State
      end;
    false ->
      %% io:format("prebuffering~n"),
      State
  end;

% base case, for events this module is not interested in
upon_event(_Other, State) ->
  %% io:format("~w ignoring event ~w~n", [?MODULE, Other]),
  State.
