-module(dancing_robots).
-export([ uses/0, upon_event/2, start_link/0, stop/0 ]).
-include_lib("wx/include/wx.hrl").
-record(state, {robot, wx}).
-record(robot, {}).

uses() -> [].

start_link() ->
  component:start_link(?MODULE).

stop() ->
  component:stop(?MODULE).


upon_event(init, _) ->
  #state{
    robot = #robot{},
    wx = start_wx()
  };

upon_event(_Other, State) ->
  %% io:format("~w ignoring event ~w~n", [?MODULE, Other]),
  State.


start_wx() ->
  WxServer = wx:new(),
  Frame = wxFrame:new(WxServer, -1, "Draw Angle", [{size, {400, 400}}]),
  Panel = wxPanel:new(Frame),
  update_onpaint({WxServer, Frame, Panel}),
  wxFrame:show(Frame),
  {WxServer, Frame, Panel}.

update_onpaint({WxServer, Frame, Panel}) ->
  OnPaint = fun(_Evt, _Obj) ->
    io:format("OnPaint~n",[]),
    Paint = wxPaintDC:new(Panel),
    Pen = wxPen:new(),
    wxPen:setColour(Pen, ?wxRED),
    wxDC:setPen(Paint, Pen),
    %% wxDC:drawCircle(Paint, {200,200}, 100),
    %% wxDC:drawArc(Paint, {300,200} , {300,200} , {200,200}),
    wxDC:drawArc(Paint, {300,200} , {100,200} , {200,200}),
    wxDC:drawArc(Paint, {100,200} , {300,200} , {200,200}),

    wxDC:drawLine(Paint, {200, 200}, {300,200}), % 100 pixels long

    wxPen:destroy(Pen),
    wxPaintDC:destroy(Paint)
  end,
  wxFrame:connect(Panel, paint, [{callback, OnPaint}]),
  wxFrame:refresh(Frame).
