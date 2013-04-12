-module(dancing_robots).
-export([ uses/0, upon_event/2, start_link/0, stop/0 ]).

-include_lib("wx/include/wx.hrl").

% graphics settings
-define(FRAME_HEIGHT, 400).
-define(FRAME_WIDTH, 400).
-define(FRAME_PADDING , 50).

-define(TEMPO, 1000).

-record(state, {robot, wx}).
-record(robot, {
  left_arm = "_", right_arm = "_", head = "o",
  x=round(?FRAME_WIDTH/2), y=round(?FRAME_HEIGHT/2), bearing=0
}).

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
  % io:format("NEXT MOVE: ~w~n", [NextStep]),
  stack:trigger({dancerobot, NextStep}),
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
  robot_new_pose(State, update_robot_position(State#state.robot, DX, DY));

upon_event({dancerobot, step_back}, State) ->
  {{dx, DX}, {dy, DY}} = step_trig(State#state.robot#robot.bearing, 10),
  % negate dx and dy to step backwards
  robot_new_pose(State, update_robot_position(State#state.robot, -DX, -DY));

upon_event({dancerobot, {left_arm, Pose}}, State) ->
  RobotNewPose = State#state.robot#robot{left_arm = arm_pose_to_str(Pose)},
  robot_new_pose(State, RobotNewPose);

upon_event({dancerobot, {right_arm, Pose}}, State) ->
  RobotNewPose = State#state.robot#robot{right_arm = arm_pose_to_str(Pose)},
  robot_new_pose(State, RobotNewPose);

upon_event({dancerobot, {head, Size}}, State) ->
  RobotNewPose = State#state.robot#robot{head = head_size_to_str(Size)},
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
    PaintDC = wxPaintDC:new(Panel),

    % draw the robot
    wxDC:drawRotatedText(PaintDC,
      Robot#robot.left_arm ++ Robot#robot.head ++ Robot#robot.right_arm,
      {Robot#robot.x, Robot#robot.y},
      Robot#robot.bearing
    ),

    % draw the stage boundary
    wxDC:drawLines(PaintDC, [
      {?FRAME_PADDING, ?FRAME_PADDING}, % top left
      {?FRAME_WIDTH - ?FRAME_PADDING, ?FRAME_PADDING}, %top right
      {?FRAME_WIDTH - ?FRAME_PADDING, ?FRAME_HEIGHT - ?FRAME_PADDING}, % bottom right
      {?FRAME_PADDING, ?FRAME_HEIGHT - ?FRAME_PADDING}, % bottom left
      {?FRAME_PADDING, ?FRAME_PADDING} % top left
    ]),
    wxPaintDC:destroy(PaintDC)
  end,
  % Install the replacment on-paint callback
  wxFrame:connect(Panel, paint, [{callback, OnPaint}]),
  % Refresh the panel to redraw with the new content
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
  DegreesToRadiansRatio = 2 * math:pi() / 360,
  DX = Distance * math:sin(Bearing * DegreesToRadiansRatio),
  DY = Distance * math:cos(Bearing * DegreesToRadiansRatio),
  {{dx, round(DX)}, {dy, round(DY)}}.

% return a (uniform) random element of the list
pick_rand(List) ->
  lists:nth(random:uniform(length(List)), List).

update_robot_position(Robot, DX, DY) ->
  Robot#robot{
    % update coords, but make sure it stays inside the box
    x = max(?FRAME_PADDING, min((?FRAME_WIDTH-?FRAME_PADDING), Robot#robot.x + DX)),
    y = max(?FRAME_PADDING, min((?FRAME_HEIGHT-?FRAME_PADDING), Robot#robot.y + DY))
  }.

