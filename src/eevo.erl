%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @TODO: 
%%% @TODO: Change time by energy, agent dies if energy =< 0.0
%%% @TODO: Old scores must be deleted as the context changes, performance?
%%% @TODO: GUI based on web
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(eevo).
-author("borja").
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

-include_lib("society.hrl").

%% API
-export([]).
-export([]).
-export_Types([]).

-type population_id() :: ruler:id().
-type agent_id()      :: agent:id().
-type rules()         :: ruler:properties().
-type features()      :: agent:properties().


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns the tables and fields used by the eevo application in
%% mnesia.
%% @end
%%--------------------------------------------------------------------
-spec attributes_table() ->
    {Table_Name :: atom(), [Fields :: atom()]}.
attributes_table() ->
    [
        {ruler, elements:fields(ruler)},
        {agent, elements:fields(agent)}
    ].

%%--------------------------------------------------------------------
%% @doc Creates a new population in the eevo database
%%
%% TODO: Indicate the available rules 
%% @end
%%--------------------------------------------------------------------
-spec population(Rules) -> Population_Id when
    Rules         :: rules(),
    Population_Id :: population_id().
population(Properties) ->
    demography:ruler(Properties).

%%--------------------------------------------------------------------
%% @doc Starts a population and its ruler
%% @end
%%--------------------------------------------------------------------
-spec start(Population :: population_id() | rules()) ->
    Population_Id :: population_id().
start(Rules) when is_map(Rules) ->
    start(population(Rules));
start(Population_Id) ->
    ok = eevo_srv:run(Population_Id),
    Population_Id.

%%--------------------------------------------------------------------
%% @doc Stops a population, the ruler and all agents
%% @end
%%--------------------------------------------------------------------
-spec stop(Population_Id :: population_id()) -> Result when
      Result :: 'ok' | {'error', Error},
      Error :: 'not_found' | 'simple_one_for_one'.
stop(Population_Id) ->
    eevo_srv:stop(Population_Id).

%%--------------------------------------------------------------------
%% @doc Creates a new population in the eevo database.
%%
%% TODO: Indicate the available options
%% @end
%%--------------------------------------------------------------------
-spec agent(Features) -> Agent_Id when
      Features :: features(),
      Agent_Id :: agent_id().
agent(Features) ->
    demography:agent(Features).

%%--------------------------------------------------------------------
%% @doc Requests a population ruler to add an agent into its population
%% @end
%%--------------------------------------------------------------------
-spec add(Population_Id, Agent_Id) -> ok when
    Population_Id :: population_id(),
    Agent_Id      :: agent_id().
add(Population_Id, Agent_Id) ->
    Ruler = ruler_pid(Population_Id),
    ruler:async_queue(Ruler, Agent_Id).

%%--------------------------------------------------------------------
%% @doc Add a score value to an agent id. The call is asynchronous.
%% @end
%%--------------------------------------------------------------------
-spec kill(Population_Id, Agent_Id) -> ok when
    Population_Id :: population_id(),
    Agent_Id      :: agent_id().
kill(Population_Id, Agent_Id) ->
    Ruler = ruler_pid(Population_Id),
    ruler:stop(Ruler, Agent_Id).

%%--------------------------------------------------------------------
%% @doc Add a score value to an agent id. The call is asynchronous.
%% @end
%%--------------------------------------------------------------------
-spec add_score(Population_Id, Agent_Id, Score) -> ok when
    Population_Id :: population_id(),
    Agent_Id      :: agent_id(),
    Score          :: float().
add_score(Population_Id, Agent_Id, Score) ->
    Ruler = ruler_pid(Population_Id),
    ruler:add_score(Ruler, Agent_Id, Score).

%%--------------------------------------------------------------------
%% @doc Sets a score value to an agent id. The call is asynchronous.
%% @end
%%--------------------------------------------------------------------
-spec set_score(Population_Id, Agent_Id, Score) -> ok when
    Population_Id :: population_id(),
    Agent_Id      :: agent_id(),
    Score          :: float().
set_score(Population_Id, Agent_Id, Score) ->
    Ruler = ruler_pid(Population_Id),
    ruler:set_score(Ruler, Agent_Id, Score).

%%--------------------------------------------------------------------
%% @doc Returns the ETS with the score table managed by a ruler.
%% @end
%%--------------------------------------------------------------------
-spec score_pool(Population_Id :: population_id()) ->
    Pool :: ets:tid().
score_pool(Population_Id) ->
    Ruler = ruler_pid(Population_Id),
    _Pool = ruler:score_pool(Ruler).

%%--------------------------------------------------------------------
%% @doc Returns the N agents with the highest score.
%% @end
%%--------------------------------------------------------------------
-spec top(Population_Id :: population_id(), N :: integer()) ->
    [{Agent_Id :: agent_id(), Score :: float(), Additional_Info :: term()}].
top(Population_Id, N) ->
    Pool = score_pool(Population_Id),
    get_last_n_from_pool(Pool, ets:last(Pool), N).

get_last_n_from_pool(_Pool, '$end_of_table', _N) -> [];
get_last_n_from_pool(_Pool, _ScoreAgent, 0)      -> [];
get_last_n_from_pool(Pool, ScoreAgent, N) ->
    [{{Score, Agent_Id}, Additional_Info}] = ets:lookup(Pool, ScoreAgent),
    [{Agent_Id, Score, Additional_Info} | get_last_n_from_pool(Pool, ets:prev(Pool, ScoreAgent), N - 1)].

%%--------------------------------------------------------------------
%% @doc Returns the N agents with the lowest score.
%% @end
%%--------------------------------------------------------------------
-spec bottom(Population_Id :: population_id(), N :: integer()) ->
    [{Agent_Id :: agent_id(), Score :: float(), Additional_Info :: term()}].
bottom(Population_Id, N) ->
    Pool = score_pool(Population_Id),
    get_first_n_from_pool(Pool, ets:first(Pool), N).

get_first_n_from_pool(_Pool, '$end_of_table', _N) -> [];
get_first_n_from_pool(_Pool, _ScoreAgent, 0)      -> [];
get_first_n_from_pool(Pool, ScoreAgent, N) ->
    [{{Score, Agent_Id}, Additional_Info}] = ets:lookup(Pool, ScoreAgent),
    [{Agent_Id, Score, Additional_Info} | get_first_n_from_pool(Pool, ets:next(Pool, ScoreAgent), N - 1)].

%%--------------------------------------------------------------------
%% @doc Returns the genealogical tree of an agent.
%% @end
%%--------------------------------------------------------------------
-spec tree(Agent_Id :: agent_id()) ->
    Genealogical_Tree :: [agent_id()].
tree(Agent_Id) ->
    demography:tree(Agent_Id).

%%--------------------------------------------------------------------
%% @doc Reads the father agent, applies the mutation function to the 
%% agent properties, saves the mutated agent on the nndb and returns 
%% its Id.
%% @end
%%--------------------------------------------------------------------
-spec mutate(Agent_Id :: agent_id()) ->
    Child_Id:: agent_id().
mutate(Agent_Id) ->
    Agent    = nndb:read(Agent_Id),
    Mutation = Agent#agent.mutation_function, 
    Child = Agent#agent{
        id         = ?AGENT_ID(nnref:new()),
        properties = Mutation(Agent#agent.properties),
        father     = Agent_Id},
    nndb:write(Child),
    Child#agent.id.

%%--------------------------------------------------------------------
%% @doc Pretty formats an eevo record.
%% @end
%%--------------------------------------------------------------------
-spec pformat(Population_Id :: population_id()) ->
    Pretty_Format :: string().
pformat(Element) ->
    _Pretty_Format = demography:pformat(Element).

%%%===================================================================
%%% Internal functions
%%%===================================================================

% --------------------------------------------------------------------
ruler_pid(Population_Id) -> 
    ets:lookup_element(?EV_POOL, Population_Id, #pop.ruler).




