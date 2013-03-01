-module(fair_loss_links).
-export([send/2, deliver/0]).

send(Q, M) ->
  Q ! M.

deliver() ->
  receive
    {{from, Sender}, Message} ->
      {Sender, Message}
  end.


%% events() ->
%%   receive
%%     {request, fll, {send, Q, M}} ->
%%       Q ! M;
%%
%%     {indication, fll, {deliver, P, M}} ->
%%       P ! M
%%   end.


%% send_request(Instance, Request) ->
%%   Sender = self(),
%%   case Request of
%%     {send, Process, Msg} ->
%%
%%   end.
%%
%%   case Indication of
%%     {deliver, Process, Msg} ->
%%
%%   end
%%
%% send(Process, Message) ->
%%   Process ! {{from, self()}, Message}.
%%

