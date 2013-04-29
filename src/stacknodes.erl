-module(stacknodes).
-behaviour(gen_server).
-export([
start_link/0, nodes/0,
init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2
]).

-record(state, {
  nodes = []
}).

% get all nodes (those present when launch_cluster_application called)
nodes() ->
  gen_server:call(?MODULE, get_nodes).


init([]) ->
  {ok, #state{}}.


start_link() ->
  %% http://erldocs.com/R15B/stdlib/gen_server.html#start_link/4
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


% set nodes to forever (lifetime of stack) contain the current [node()|erlang:nodes()
handle_call(lock_nodes, _From, State) ->
  StateWithNodes = State#state{nodes = lists:sort([node() | erlang:nodes()] )},
  {reply, StateWithNodes#state.nodes, StateWithNodes};


% return all nodes known at stack launch (including ones who have since crashed)
handle_call(get_nodes, _From, State) ->
  {reply, State#state.nodes, State};

handle_call(stop, _From, State) ->
  {stop, normal, ok, State}.




handle_cast(Request, State) ->
  {stop, {unexpected_message, Request}, State}.

%% NOT USED

handle_info(_,State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(normal, _State) ->
  ok.
