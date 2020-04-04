%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(demography).
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

-include_lib("eunit/include/eunit.hrl").

%% API
%%-export([]).
-export_type([type/0, ruler/0, agent/0]).
-export_type([]).

%%% Populations are composed mostly by 2 types:
-type type()   ::      ruler |      agent.
-type id()     :: ruler:id() | agent:id().

-define(NEW_RULER_ID,       {make_ref(), ruler}).
-define(NEW_AGENT_ID,       {make_ref(), agent}).
-define(REFERENCE(Id), element(2,element(1,Id))).



-record(ruler, {
    id = ?NEW_RULER_ID :: ruler:id(),
    maximum    = 5     :: integer(), % Max agents per population
    minimum    = 4     :: integer(), % Min agents per population
    run_time   = infinity :: integer(), % Milliseconds of running time before pausing the population
    run_agents = infinity :: integer(), % Number of Agents of tested before pausing the population
    run_score  = infinity :: float(), % Score target before pausing the population
    evo_alg_f  = fun evolutionary_algorithm:static_on_limit/2 :: function(),
    sel_alg_f  = fun selection_algorithm:statistic_top_3/2 :: function()
}).
-type ruler() :: #ruler{}.

-record(agent, {
    id :: agent:id()(),
    module :: module(),
    properties :: #{},
    mutation_f :: function(),
    behaviour = gen_server :: module(),
    father = none :: agent_id() % Id of the agent one generation back
}). 
-type agent() :: #agent{}.




















%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a new population on nndb and returns its Id
%%
%% @end
%%--------------------------------------------------------------------
new_population(Options) ->
    Population = population(Options),
    nndb:write(Population),
    Population#population.id.

%%--------------------------------------------------------------------
%% @doc
%% Creates a new agent on nndb and returns its Id
%%
%% @end
%%--------------------------------------------------------------------
new_agent(Module, AgentProperties, MutationF, Options) ->
    Agent = agent(Module, AgentProperties, MutationF, Options),
    nndb:write(Agent),
    Agent#agent.id.

%%--------------------------------------------------------------------
%% @doc
%% TODO: Make description
%%
%% @end
%%--------------------------------------------------------------------
tree(none) ->
    [];
tree(Agent_Id) ->
    Agent = #agent{} = nndb:read(Agent_Id),
    [Agent_Id | tree(Agent#agent.father)].

%%--------------------------------------------------------------------
%% @doc
%% TODO: Make description
%%
%% @end
%%--------------------------------------------------------------------
element_id(Element) when is_record(Element, agent)      -> Element#agent.id;
element_id(Element) when is_record(Element, population) -> Element#population.id.

%%--------------------------------------------------------------------
%% @doc
%% TODO: Make description
%%
%% @end
%%--------------------------------------------------------------------
pformat(Element) when is_record(Element, agent)      -> pformat(Element, record_info(fields, agent));
pformat(Element) when is_record(Element, population) -> pformat(Element, record_info(fields, population)).

pformat(Element, Fields) ->
    [io_lib:format("Element record: ~w ~n", [element(1, Element)]) |
     pformat(Element, Fields, 2)].

pformat(Element, [Field | ListOf_Fields], Index) ->
    [io_lib:format(" --> ~w = ~w ~n", [Field, element(Index, Element)]) |
     pformat(Element, ListOf_Fields, Index + 1)];
pformat(_Element, [], _Index) ->
    [].

%%====================================================================
%% Internal functions
%%====================================================================

%--------------------------------------------------------------------...................................................
population(Options) ->
    Population = #population{
        id = ?POPULATION_ID(nnref:new())
    },
    write_population_options(Population, Options).

write_population_options(Population, [{name, Value} | Options]) ->
    write_population_options(Population#population{id = ?POPULATION_ID(Value)}, Options);
write_population_options(Population, [{limit, Value} | Options]) ->
    write_population_options(Population#population{limit = Value}, Options);
write_population_options(Population, [{minimum, Value} | Options]) ->
    write_population_options(Population#population{minimum = Value}, Options);
write_population_options(Population, [{run_time, Value} | Options]) ->
    write_population_options(Population#population{run_time = Value}, Options);
write_population_options(Population, [{run_agents, Value} | Options]) ->
    write_population_options(Population#population{run_agents = Value}, Options);
write_population_options(Population, [{run_score, Value} | Options]) ->
    write_population_options(Population#population{run_score = Value}, Options);
write_population_options(Population, [{evo_alg_f, Value} | Options]) ->
    write_population_options(Population#population{evo_alg_f = Value}, Options);
write_population_options(Population, [{sel_alg_f, Value} | Options]) ->
    write_population_options(Population#population{sel_alg_f = Value}, Options);
write_population_options(Population, []) ->
    Population.

% --------------------------------------------------------------------..................................................
agent(Module, AgentProperties, MutationF, Options) ->
    Agent = #agent{
        id         = ?AGENT_ID(nnref:new()),
        module     = Module,
        properties = AgentProperties,
        mutation_f = MutationF
    },
    write_agent_options(Agent, Options).

write_agent_options(Agent, [{name, Value} | Options]) ->
    write_agent_options(Agent#agent{id = ?AGENT_ID(Value)}, Options);
write_agent_options(Agent, [{behaviour, Value} | Options]) ->
    write_agent_options(Agent#agent{behaviour = Value}, Options);
write_agent_options(Agent, [{father, Value} | Options]) ->
    write_agent_options(Agent#agent{father = Value}, Options);
write_agent_options(Agent, []) ->
    Agent.

%%====================================================================
%% Eunit white box tests
%%====================================================================





