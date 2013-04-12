-module(dancing_robots).
-export([ uses/0, upon_event/2, start_link/0, stop/0 ]).
-include_lib("wx/include/wx.hrl").
-record(state, {robot, wx}).
-record(robot, {left_arm = "_", right_arm = "_", head = "o"}).

uses() -> [].

start_link() ->
  component:start_link(?MODULE).

stop() ->
  component:stop(?MODULE).


% TODO: timer which picks and broadcasts next random move, from every robot

upon_event(init, _) ->
  Robot = #robot{},
  Wx = start_wx(Robot),
  #state{
    robot = Robot,
    wx = Wx
  };

upon_event({dancerobot, left_arm_up}, State) ->
  RobotNewPose = #robot{left_arm = "\\"},
  robot_new_pose(State, RobotNewPose);

upon_event({dancerobot, left_arm_down}, State) ->
  RobotNewPose = #robot{left_arm = "_"},
  robot_new_pose(State, RobotNewPose);

upon_event({dancerobot, head_stretch}, State) ->
  RobotNewPose = #robot{head = "0"},
  robot_new_pose(State, RobotNewPose);

upon_event({dancerobot, head_relax}, State) ->
  RobotNewPose = #robot{head = "o"},
  robot_new_pose(State, RobotNewPose);

upon_event({dancerobot, right_arm_up}, State) ->
  RobotNewPose = #robot{right_arm = "/"},
  robot_new_pose(State, RobotNewPose);

upon_event({dancerobot, right_arm_down}, State) ->
  RobotNewPose = #robot{right_arm = "_"},
  robot_new_pose(State, RobotNewPose);

upon_event(_Other, State) ->
  %% io:format("~w ignoring event ~w~n", [?MODULE, Other]),
  State.


start_wx(Robot) ->
  WxServer = wx:new(),
  Frame = wxFrame:new(WxServer, -1, io_lib:format("Robot ~w", [node()]), [{size, {300, 300}}]),
  Panel = wxPanel:new(Frame),
  % draw intial robot
  update_onpaint({WxServer, Frame, Panel}, Robot),
  wxFrame:connect(Frame, close_window, [{callback, fun(_Evt, _Obj) -> stack:stop() end}]),
  wxFrame:show(Frame),
  {WxServer, Frame, Panel}.

% repaint panel with current robot position and pose
update_onpaint({WxServer, Frame, Panel}, Robot) ->
  OnPaint = fun(_Evt, _Obj) ->
    io:format("OnPaint~n",[]),
    PaintDC = wxPaintDC:new(Panel),
    wxDC:drawRotatedText(PaintDC,
      Robot#robot.left_arm ++ Robot#robot.head ++ Robot#robot.right_arm,
      {100, 100},
      45
    ),
    wxPaintDC:destroy(PaintDC)
  end,
  % install the replacment on-paint callback
  % & refresh the panel to redraw with the new content
  wxFrame:connect(Panel, paint, [{callback, OnPaint}]),
  wxFrame:refresh(Frame).


% update state and draw new robot to screen
robot_new_pose(State, RobotNewPose) ->
  update_onpaint(State#state.wx, RobotNewPose),
  State#state{robot = RobotNewPose}.
