-module(dancing_robots).
-export([ uses/0, upon_event/2, start_link/0, stop/0, start_dance/0, stop_dance/0 ]).

-include_lib("wx/include/wx.hrl").

% graphics settings
-define(FRAME_HEIGHT, 400).
-define(FRAME_WIDTH, 400).
-define(FRAME_PADDING , 50).

-define(TEMPO, 1000).
-define(STEPS, 50).


-record(state, {proposing_moves = false, step=0, steps=[], robot, wx}).
-record(robot, {
  left_arm = "_", right_arm = "_", head = "o",
  x=round(?FRAME_WIDTH/2), y=round(?FRAME_HEIGHT/2), bearing=0
}).

uses() -> [beb].

% insert the trigger for the chosen broadcast abstraction here, e.g. beb
broadcast(Msg) ->
  stack:trigger({beb, broadcast, {dancerobot, Msg}}).

% catch any broadcast from the chosen broadcast mechanism
% change beb to match your choice of broadcast algo
upon_event({beb, deliver, _Sender, {dancerobot, Msg}}, State) ->
  io:format("ROBOT DELIVER MSG ~w~n", [Msg]),
  process_broadcast(Msg, State);

%% upon_event({tob, deliver, _Sender, Msg}, State) ->
%%   io:format("ROBOT DELIVER TOB MSG ~w~n", [Msg]);
%%   %% process_broadcast(Msg, State);

% initialise to a state that shows the robot and is ready to receive moves
upon_event(init, _) ->
  component:start_timer(?TEMPO),
  Robot = #robot{},
  Wx = start_wx(Robot),
  #state{
    proposing_moves = false,
    robot = Robot,
    wx = Wx
  };

% not yet proposing_moves, but trigger next timer
upon_event(timeout, State=#state{proposing_moves = false}) ->
  component:start_timer(?TEMPO),
  State;

% propose 100 moves then stop
upon_event(timeout, State=#state{proposing_moves = true}) when State#state.step >= ?STEPS ->
  % do not launch new timer: stop heartbeats, stop proposing_moves
  io:format("~w STOPPED DANCING~n", [node()]),
  State#state{proposing_moves=false};

% upon timeout propose the next dance move to the group
upon_event(timeout, State=#state{proposing_moves = true}) ->
  NextStep = case pick_rand([turn_left, turn_right,
    step_forward, step_back,
    head, left_arm, right_arm]
  ) of
    head -> {head, rand_head_size()};
    left_arm -> {left_arm, rand_arm_pose()};
    right_arm -> {right_arm, rand_arm_pose()};
    Others -> Others
  end,
  % io:format("NEXT MOVE: ~w~n", [NextStep]),
  broadcast(NextStep),
  % continue proposing_moves
  component:start_timer(?TEMPO),
  State;

% handle request query for the current steps and position of the robot
upon_event({dancerobot, get_steps}, State) ->
  io:format("Robot ~w~n***Position: ~w~n***Steps:~w~n", [node(), State#state.robot, State#state.steps]),
  State;

upon_event(start_application, State) ->
  % io:format("~w starting to propose moves~n", [node()]),
  State#state{proposing_moves = true};

upon_event(_Other, State) ->
  %% io:format("~w ignoring event ~w~n", [?MODULE, Other]),
  State.



% once n steps taken stop dancing, ignoring any more received steps
process_broadcast(Move, State) when State#state.step >= ?STEPS ->
  io:format("~w dropping move ~w~n", [node(), Move]),
  State;

process_broadcast(Move=turn_left, State) ->
  NextRobot = State#state.robot#robot{
    bearing = ((State#state.robot#robot.bearing - 45) rem 360)
  },
  update_robot(State, NextRobot, Move);

process_broadcast(Move=turn_right, State) ->
  NextRobot = State#state.robot#robot{
    bearing = ((State#state.robot#robot.bearing + 45) rem 360)
  },
  update_robot(State, NextRobot, Move);

process_broadcast(Move=step_forward, State) ->
  {{dx, DX}, {dy, DY}} = step_trig(State#state.robot#robot.bearing, 10),
  update_robot(State, robot_step_to(State#state.robot, DX, DY), Move);

process_broadcast(Move=step_back, State) ->
  {{dx, DX}, {dy, DY}} = step_trig(State#state.robot#robot.bearing, 10),
  % negate dx and dy to step backwards
  update_robot(State, robot_step_to(State#state.robot, -DX, -DY), Move);

process_broadcast( Move={left_arm, Pose}, State) ->
  NextRobot = State#state.robot#robot{left_arm = arm_pose_to_str(Pose)},
  update_robot(State, NextRobot, Move);

process_broadcast(Move={right_arm, Pose}, State) ->
  NextRobot = State#state.robot#robot{right_arm = arm_pose_to_str(Pose)},
  update_robot(State, NextRobot, Move);

process_broadcast(Move={head, Size}, State) ->
  NextRobot = State#state.robot#robot{head = head_size_to_str(Size)},
  update_robot(State, NextRobot, Move).


% update state and draw new robot to screen
% record taken dance move to the steps list
update_robot(State, NextRobot, Step) ->
  update_onpaint(State#state.wx, NextRobot),
  State#state{robot = NextRobot,
    steps = lists:append([State#state.steps, [Step]]),
    step = (State#state.step + 1)
  }.

robot_step_to(Robot, DX, DY) ->
  Robot#robot{
    % update coords, but make sure it stays inside the box
    x = max(?FRAME_PADDING, min((?FRAME_WIDTH-?FRAME_PADDING), Robot#robot.x + DX)),
    y = max(?FRAME_PADDING, min((?FRAME_HEIGHT-?FRAME_PADDING), Robot#robot.y + DY))
  }.

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




start_link() ->
  component:start_link(?MODULE).

stop() ->
  component:stop(?MODULE).

start_dance() ->
  broadcast(start_dance).

stop_dance() ->
  broadcast(stop_dance).


% initialise the gui and draw the first robot position
start_wx(Robot) ->
  WxServer = wx:new(),
  Frame = wxFrame:new(WxServer, -1, io_lib:format("Robot ~w", [node()]), [{size, {?FRAME_WIDTH, ?FRAME_HEIGHT}}]),
  Panel = wxPanel:new(Frame),
  % draw intial robot
  update_onpaint({WxServer, Frame, Panel}, Robot),
  wxFrame:connect(Frame, close_window, [{callback, fun(_Evt, _Obj) -> stack:stop() end}]),
  wxFrame:show(Frame),
  {WxServer, Frame, Panel}.

% update the gui
% for use before the dance finishes
update_onpaint({_WxServer, Frame, Panel}, Robot) ->
  update_onpaint({_WxServer, Frame, Panel}, Robot, false).
% update the gui
% repaint panel with current robot position and pose
update_onpaint({_WxServer, Frame, Panel}, Robot, DanceCompleted) ->
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

    %draw FINISHED if it has
    case DanceCompleted of
      true ->
        wxDC:drawText(PaintDC, "FINISHED", {100, 100});
      false ->
        ok
    end,

    wxPaintDC:destroy(PaintDC)
  end,
  % Install the replacment on-paint callback
  wxFrame:connect(Panel, paint, [{callback, OnPaint}]),
  % Refresh the panel to redraw with the new content
  wxFrame:refresh(Frame).

