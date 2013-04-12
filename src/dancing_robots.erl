-module(dancing_robots).
-export([ uses/0, upon_event/2, start_link/0, stop/0 ]).

-include_lib("wx/include/wx.hrl").

-record(state, {robot, wx}).
-record(robot, {
  left_arm = "_", right_arm = "_", head = "o",
  x=100, y=100, bearing=0
}).

-define(TEMPO, 2000).
-define(FRAME_HEIGHT, 400).
-define(FRAME_WIDTH, 400).

uses() -> [].

start_link() ->
  component:start_link(?MODULE).

stop() ->
  component:stop(?MODULE).


upon_event(init, _) ->
  component:start_timer(?TEMPO),
  Robot = #robot{},
  Wx = start_wx(Robot),
  #state{
    robot = Robot,
    wx = Wx
  };

upon_event(timeout, State) ->
  % pick a dance move and send to the group
  NextStep = case pick_rand([turn_left, turn_right,
    step_forward, step_back,
    head, left_arm, right_arm]
  ) of
    head -> {head, rand_head_size()};
    left_arm -> {left_arm, rand_arm_pose()};
    right_arm -> {right_arm, rand_arm_pose()};
    Others -> Others
  end,
  % TODO: broadcast next step
  State;


upon_event({dancerobot, turn_left}, State) ->
  RobotNewPose = State#state.robot#robot{
    bearing = ((State#state.robot#robot.bearing - 45) rem 360)
  },
  robot_new_pose(State, RobotNewPose);

upon_event({dancerobot, turn_right}, State) ->
  RobotNewPose = State#state.robot#robot{
    bearing = ((State#state.robot#robot.bearing + 45) rem 360)
  },
  robot_new_pose(State, RobotNewPose);

upon_event({dancerobot, step_forward}, State) ->
  {{dx, DX}, {dy, DY}} = step_trig(State#state.robot#robot.bearing, 10),
  RobotNewPose = State#state.robot#robot{
    % update coords, but make sure it stays inside the box
    x = max(0, min(?FRAME_WIDTH, State#state.robot#robot.x + DX)),
    y = max(0, min(?FRAME_HEIGHT, State#state.robot#robot.y + DY))
  },
  robot_new_pose(State, RobotNewPose);

upon_event({dancerobot, step_back}, State) ->
  % negate dx and dy to step backwards
  {{dx, DX}, {dy, DY}} = step_trig(State#state.robot#robot.bearing, 10),
  RobotNewPose = State#state.robot#robot{
    % update coords, but make sure it stays inside the box
    x = max(0, min(?FRAME_WIDTH, State#state.robot#robot.x - DX)),
    y = max(0, min(?FRAME_HEIGHT, State#state.robot#robot.y - DY))
  },
  robot_new_pose(State, RobotNewPose);

upon_event({dancerobot, {left_arm, Pose}}, State) ->
  RobotNewPose = #robot{left_arm = arm_pose_to_str(Pose)},
  robot_new_pose(State, RobotNewPose);

upon_event({dancerobot, {right_arm, Pose}}, State) ->
  RobotNewPose = #robot{right_arm = arm_pose_to_str(Pose)},
  robot_new_pose(State, RobotNewPose);

upon_event({dancerobot, {head, Size}}, State) ->
  RobotNewPose = #robot{head = head_size_to_str(Size)},
  robot_new_pose(State, RobotNewPose);

upon_event(_Other, State) ->
  %% io:format("~w ignoring event ~w~n", [?MODULE, Other]),
  State.


start_wx(Robot) ->
  WxServer = wx:new(),
  Frame = wxFrame:new(WxServer, -1, io_lib:format("Robot ~w", [node()]), [{size, {?FRAME_WIDTH, ?FRAME_HEIGHT}}]),
  Panel = wxPanel:new(Frame),
  % draw intial robot
  update_onpaint({WxServer, Frame, Panel}, Robot),
  wxFrame:connect(Frame, close_window, [{callback, fun(_Evt, _Obj) -> stack:stop() end}]),
  wxFrame:show(Frame),
  {WxServer, Frame, Panel}.

% repaint panel with current robot position and pose
update_onpaint({_WxServer, Frame, Panel}, Robot) ->
  OnPaint = fun(_Evt, _Obj) ->
    io:format("OnPaint~n",[]),
    PaintDC = wxPaintDC:new(Panel),
    wxDC:drawRotatedText(PaintDC,
      Robot#robot.left_arm ++ Robot#robot.head ++ Robot#robot.right_arm,
      {Robot#robot.x, Robot#robot.y},
      Robot#robot.bearing
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

% return a random arm pose
rand_arm_pose() ->
  pick_rand([up_left, up_right, down]).

% map arm pose symbols to strings
arm_pose_to_str(Pose) ->
  case Pose of
    up_left  -> "\\";
    up_right -> "/";
    down -> "_"
  end.

% return a random head size
rand_head_size() ->
  pick_rand([stretch, shrink, relax, big]).

% map head size symbols to strings
head_size_to_str(Size) ->
  case Size of
    stretch  -> "0";
    shrink -> ".";
    relax -> "o";
    big -> "O"
  end.


% returns {change in x, change in y} for a length Distance step when facing Bearing degrees
step_trig(Bearing, Distance) ->
  {{dx, Distance * math:sin(Bearing)}, {dy, Distance * math:cos(Bearing)}}.

% return a (uniform) random element of the list
pick_rand(List) ->
  lists:nth(random:uniform(length(List)), List).
