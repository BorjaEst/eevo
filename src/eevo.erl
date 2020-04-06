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
-type summary()       :: #{Field :: info() => Value :: term()}.
-type info()          :: size | runtime | generation | score | 
                         top3.


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
        {ruler, demography:fields(ruler)},
        {agent, demography:fields(agent)}
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
    Ruler = demography:ruler(Properties),
    edb:write(Ruler),
    demography:id(Ruler).

%%--------------------------------------------------------------------
%% @doc Starts and runs a population acording to some limits. 
%% @end
%%--------------------------------------------------------------------
-spec start(Population, Seeds) -> Population_Id when
    Population    :: population_id() | rules(), 
    Seeds         :: [agent:id()],
    Population_Id :: population_id().
start(Rules, Seeds) when is_map(Rules) ->
    start(population(Rules), Seeds);

start(Population_Id, Seeds) ->
    case eevo_sup:start_population(Population_Id) of 
        {   ok,                 Pid } -> Ruler = Pid;
        {error,{already_started,Pid}} -> Ruler = Pid
    end,
    [ok = ruler:async_queue(Ruler,Id) || Id <- Seeds],
    ok  = ruler:run(Ruler),
    Population_Id.

%%--------------------------------------------------------------------
%% @doc Stops a population killing the the ruler and all agents.
%% @end
%%--------------------------------------------------------------------
-spec stop(Population_Id :: population_id()) -> Result when
      Result :: 'ok' | {'error', Error},
      Error :: 'not_found' | 'simple_one_for_one'.
stop(Population_Id) ->
    eevo_sup:stop_population(Population_Id).

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
    Agent = demography:agent(Features),
    edb:write(Agent),
    demography:id(Agent).

%%--------------------------------------------------------------------
%% @doc Requests a population ruler to add an agent into its 
%% population.
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
-spec score(Pid, Score) -> ok when
    Pid   :: population_id(),
    Score :: float().
score(Pid, Score) ->
    [DNI] = ets:lookup(?AGENTS_POOL, Pid),
    score(DNI#dni.population_id, DNI#dni.agent_id, Score).

-spec score(Population_Id, Agent_Id, Score) -> ok when
    Population_Id :: population_id(),
    Agent_Id      :: agent_id(),
    Score          :: float().
score(Population_Id, Agent_Id, Score) ->
    Ruler = ruler_pid(Population_Id),
    ruler:score(Ruler, Agent_Id, Score).

%%--------------------------------------------------------------------
%% @doc Returns the ETS with the score table managed by a ruler.
%% @end
%%--------------------------------------------------------------------
-spec score_pool(Population_Id :: population_id()) ->
    Pool :: ets:tid().
score_pool(Population_Id) ->
    ruler:score_pool(Population_Id).

%%--------------------------------------------------------------------
%% @doc Returns the N agents with the highest score.
%% @end
%%--------------------------------------------------------------------
-spec top(Population_Id :: population_id(), N :: integer()) ->
    [{Agent_Id :: agent_id(), Score :: float()}].
top(Population_Id, N) ->
    ruler:top(score_pool(Population_Id), N).

%%--------------------------------------------------------------------
%% @doc Returns the N agents with the lowest score.
%% @end
%%--------------------------------------------------------------------
-spec bottom(Population_Id :: population_id(), N :: integer()) ->
    [{Agent_Id :: agent_id(), Score :: float()}].
bottom(Population_Id, N) ->
    ruler:bottom(score_pool(Population_Id), N).


%%--------------------------------------------------------------------
%% @doc Returns status of a population id.
%% @end
%%--------------------------------------------------------------------
-spec status(Population_Id :: population_id()) ->
    Status :: summary().
status(Population_Id) ->
    case ets:lookup(?EV_POOL, Population_Id) of 
        [Population] -> pop2status(Population);
        []           -> {error, not_found}
    end.

pop2status(Population) ->
    #{
        size       => Population#population.size,
        runtime    => Population#population.runtime,
        generation => Population#population.generation,
        score      => Population#population.score,
        top3       => top(Population#population.id, 3)
    }.

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
    Agent = edb:read(Agent_Id),
    Child = demography:mutate_agent(Agent),
    edb:write(Child),
    demography:id(Child).

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
    ets:lookup_element(?EV_POOL, Population_Id, #population.ruler).

