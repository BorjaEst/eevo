%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(agent).
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").

% -export([]). 
-export_type([id/0, property/0, features/0]).

-type id() :: {agent, Ref :: integer()}.
-define(NEW_KEY, erlang:unique_integer([monotonic, positive])).
-type property() :: function | arguments | mutation | father.
-type features() :: #{function  := Value :: term(),
                      arguments := Value :: term(),
                      mutation  := Value :: term()}.
-record(agent, {
    key = ?NEW_KEY :: reference(),
    parent = root  :: agent:id(), % Id of the agent one generation back
    function       :: function(), % Function to perform
    arguments      :: term(),     % Arguments of the function
    mutation       :: function()  % Mutation to modify the arguments
}). 

-define(  DEF_SELECTION,     top3).
-define(   DEF_MAX_SIZE,      100).
-define(  DEF_STOP_TIME,      100).
-define(DEF_GENERATIONS, infinity).
-define(     DEF_TARGET, infinity).

-record(state, {
    id         :: id(),
    sgroup     :: scorer:group()
}).
-define(       ID, State#state.id       ).
-define(   SGROUP, State#state.sgroup   ).
-define( FUNCTION, State#state.function ).
-define(ARGUMENTS, State#state.arguments).


%%%===================================================================
%%% API
%%%===================================================================

%%-------------------------------------------------------------------
%% @doc Record fields of an agent record.  
%% Should run inside an mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec record_fields() -> ListOfFields :: [atom()].
record_fields() -> record_info(fields, agent).

%%--------------------------------------------------------------------
%% @doc Creates a new agent.  
%% Should run inside an mnesia transaction.
%% @end
%%--------------------------------------------------------------------
-spec new(Info::features()) -> Id::id().
new(Features) ->
    Agent = #agent{
        function  = maps:get( function, Features),
        arguments = maps:get(arguments, Features),
        mutation  = maps:get( mutation, Features)
    },
    ok  = mnesia:write(Agent),
    {agent, Agent#agent.key}.

%%--------------------------------------------------------------------
%% @doc Clones an agent. The id is asigned to the parent field and a
%% new id is generated.
%% Should run inside an mnesia transaction.
%% @end
%%--------------------------------------------------------------------
-spec clone(Id::id()) -> Clone::id().
clone(Id) ->
    Agent = read(Id),
    Clone = Agent#agent{key=?NEW_KEY, parent=Id},
    ok = mnesia:write(Clone),
    {agent, Clone#agent.key}.

%%--------------------------------------------------------------------
%% @doc Returns the agent parent.  
%% Should run inside an mnesia transaction.
%% @end
%%--------------------------------------------------------------------
-spec parent(Id::id()) -> Parent::id().
parent(Id) -> 
    Agent = read(Id),
    Agent#agent.parent.

%%--------------------------------------------------------------------
%% @doc Returns the agent parent.  
%% @end
%%--------------------------------------------------------------------
-spec features(Id::id()) -> Info::features().
features(Id) ->
    Agent = read(Id),
    #{function  => Agent#agent.function,
      arguments => Agent#agent.arguments,
      mutation  => Agent#agent.mutation}.

%%--------------------------------------------------------------------
%% @doc Mutates an agent (The id keeps the same).
%% Should run inside an mnesia transaction.
%% @end
%--------------------------------------------------------------------
-spec mutate(Id::id()) -> ok.
mutate(Id) ->
    update(Id, fun do_mutation/1).

do_mutation(#agent{mutation=Fun, arguments=Arg} = Agent) ->
    Agent#agent{arguments=apply(Fun, Arg)}.

%%--------------------------------------------------------------------
%% @doc Spawn function for the population supervisor.
%% @end
%%--------------------------------------------------------------------
-spec start_link(Agent_Id :: id(), scorer:group()) -> 
    {ok, Pid :: pid()}.
start_link(Id, ScoreGroup) ->
    #agent{function=Function, arguments=Arguments} = dirty_read(Id),
    State = #state{id=Id, sgroup=ScoreGroup},
    {ok, spawn_link(?MODULE, loop, [Function, Arguments, State])}.


%%%===================================================================
%%% Callback functions
%%%===================================================================

% Call for the agent init -------------------------------------------
init(Id, ScoreGroup, Agent) ->
    ?LOG_INFO(#{what=>"Initialising agent", id=>Id, gp=>ScoreGroup}), 
    loop(Agent#agent.function, Agent#agent.arguments, #state{
        id = Id, 
        sgroup = ScoreGroup
    }).

% Call for the agent loop -------------------------------------------
loop(Function, Arguments, State) -> 
    ?LOG_DEBUG(#{what=>"Run agent loop", arg=>Arguments, func=>Function}),
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

% Reads the agent from mnesia ---------------------------------------
read(Id) -> 
    case mnesia:read(Id) of 
        [Agent] -> Agent;
         []     -> error(not_found)
    end.

% Reads the agent from mnesia ---------------------------------------
dirty_read(Id) -> 
    case mnesia:dirty_read(Id) of 
        [Agent] -> Agent;
         []     -> error(not_found)
    end.

% Updates the agent in mnesia ---------------------------------------
update(Id, Function) -> 
    case mnesia:wread(Id) of 
        [Agent] -> ok = mnesia:write(Function(Agent));
         []     -> error(not_found)
    end.

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
