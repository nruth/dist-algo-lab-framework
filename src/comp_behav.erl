-module (comp_behav).
 
-callback upon_event(Event :: atom(), State :: any()) -> NewState :: term().

-callback uses() -> list(atom()).
