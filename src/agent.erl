%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(agent).
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

% -export([]). 
-export_type([id/0, agent/0, property/0, features/0]).

-type id() :: {Ref :: reference(), agent}.
% -define(NEW_ID, {make_ref(), agent}).
-define(NEW_ID, {erlang:unique_integer([monotonic, positive]), agent}).
-type property() :: function | arguments | mutation | father.
-type features() :: #{function  := Value :: term(),
                      arguments := Value :: term(),
                      mutation  := Value :: term()}.
-record(agent, {
    id = ?NEW_ID  :: agent:id(),
    parent = root :: agent:id(), % Id of the agent one generation back
    function      :: function(), % Function to perform
    arguments     :: term(),     % Arguments of the function
    mutation      :: function()  % Mutation to modify the arguments
}). 
-type agent() :: #agent{}.

-define(  DEF_SELECTION,     top3).
-define(   DEF_MAX_SIZE,      100).
-define(  DEF_STOP_TIME,      100).
-define(DEF_GENERATIONS, infinity).
-define(     DEF_TARGET, infinity).

-record(state, {
    id         :: id(),
    sgroup     :: scorer:group(),
    function   :: function(),  % Function to perform
    arguments  :: [term()]     % Arguments to 
}).
-define(       ID, State#state.id       ).
-define(   SGROUP, State#state.sgroup   ).
-define( FUNCTION, State#state.function ).
-define(ARGUMENTS, State#state.arguments).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new agent.  
%% @end
%%--------------------------------------------------------------------
-spec new(features()) -> agent().
new(Features) ->
    #agent{
        function  = maps:get( function, Features),
        arguments = maps:get(arguments, Features),
        mutation  = maps:get( mutation, Features)
    }.

%%--------------------------------------------------------------------
%% @doc Clones an agent. The id is asigned to the parent field and a
%% new id is generated.
%% @end
%%--------------------------------------------------------------------
-spec clone(agent()) -> agent().
clone(Agent) ->
    Agent#agent{
        id     = ?NEW_ID,
        parent = Agent#agent.id
    }.

%%--------------------------------------------------------------------
%% @doc Returns the agent id.  
%% @end
%%--------------------------------------------------------------------
-spec id(agent()) -> id().
id(Agent) -> Agent#agent.id.

%%-------------------------------------------------------------------
%% @doc Record fields of an agent record.  
%% @end
%%-------------------------------------------------------------------
-spec record_fields() -> ListOfFields :: [atom()].
record_fields() -> record_info(fields, agent).

%%--------------------------------------------------------------------
%% @doc Returns the agent parent.  
%% @end
%%--------------------------------------------------------------------
-spec parent(agent()) -> id().
parent(Agent) -> Agent#agent.parent.

%%--------------------------------------------------------------------
%% @doc Mutates an agent (The id keeps the same).
%% @end
%--------------------------------------------------------------------
-spec mutate(Agent :: agent()) -> 
    Mutated :: agent().
mutate(Agent) -> 
    Mutation_Function = Agent#agent.mutation,
    Agent#agent{
        arguments = Mutation_Function(Agent#agent.arguments)
    }.

%%--------------------------------------------------------------------
%% @doc Spawn function for the population supervisor.
%% @end
%%--------------------------------------------------------------------
-spec start_link(Agent_Id :: id(), scorer:group()) -> 
    {ok, Pid :: pid()}.
start_link(Id, ScoreGroup) ->
    [Agent]   = mnesia:dirty_read(agent, Id),
    Function  = Agent#agent.function,
    Arguments = Agent#agent.arguments,
    State = #state{id=Id, sgroup=ScoreGroup},
    {ok, spawn_link(?MODULE, loop, [Function, Arguments, State])}.

%%--------------------------------------------------------------------
%% @doc Spawn function for non OTP run.
%% @end
%%--------------------------------------------------------------------
-spec start(Agent_Id :: id()) -> 
    {ok, Pid :: pid()}.
start(Id) ->
    {ok, ScoreGroup} = application:get_env(eevo, shell_sgroup),
    start(Id, ScoreGroup).

-spec start(Agent_Id :: id(), scorer:group()) -> 
    {ok, Pid :: pid()}.
start(Id, ScoreGroup) ->
    [Agent]   = mnesia:dirty_read(agent, Id),
    Function  = Agent#agent.function,
    Arguments = Agent#agent.arguments,
    State = #state{id=Id, sgroup=ScoreGroup},
    {ok, spawn(?MODULE, loop, [Function, Arguments, State])}.


%%%===================================================================
%%% Callback functions
%%%===================================================================

% Call for the agent loop -------------------------------------------
loop(Function, Arguments, State) -> 
    case apply(Function, Arguments) of 
        {next, Fun, Arg         } -> loop(Fun, Arg, State);
        {next, Fun, Arg, Actions} -> loop(Fun, Arg, actions(Actions, State));
        {stop,   Reason         } -> exit(Reason);
        {stop,   Reason, Actions} -> actions(Actions, State), exit(Reason);
        Other -> error({"Wrong agent return", Other})
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

% Call for the actions list -----------------------------------------
actions([{score, Points} | Actions], State) -> 
    scorer:add_score(?SGROUP, ?ID, Points),
    actions(Actions, State);

actions([], State) -> 
    State.


%%====================================================================
%% Eunit white box tests
%%====================================================================

% --------------------------------------------------------------------
% TESTS DESCRIPTIONS -------------------------------------------------

% --------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS -------------------------------------------

% --------------------------------------------------------------------
% ACTUAL TESTS -------------------------------------------------------

% --------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS ------------------------------------------
