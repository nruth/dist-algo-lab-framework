-module(stack_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _Args) ->
  stack:start_link().

stop(_State) ->
  stack:stop().

