%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(demography).
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

-include_lib("eunit/include/eunit.hrl").
-include_lib("society.hrl").

%% API
%%-export([]).

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
genealogical_tree(none) ->
	[];
genealogical_tree(Agent_Id) ->
	Agent = #agent{} = nndb:read(Agent_Id),
	[Agent_Id | genealogical_tree(Agent#agent.father)].

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

%.......................................................................................................................
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

% ......................................................................................................................
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





