%%%-------------------------------------------------------------------
%%% @author borja
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Sep 2018 20:14
%%%-------------------------------------------------------------------

-define(DEFAULT_RESULTS_PATH, "population_data").

-define(POPULATION_ID(Name), {Name, population}).
-define(AGENT_ID(Name), {Name, agent}).
-define(ID_FILEMODE(Id),  nnref:list(element(1,Id)) ++ atom_to_list(element(2,Id))).

-define(population(Properties), maps:get(population_id, Properties)).
-define(agent_id(Properties), maps:get(agent_id, Properties)).

-define(add_score(Properties, Score), eevo:add_score(?population(Properties), ?agent_id(Properties), Score)).
-define(set_score(Properties, Score), eevo:set_score(?population(Properties), ?agent_id(Properties), Score)).
-define(end_agent(Properties), exit(end_agent)).

-define(add_parameter(Key, Value, Properties), maps:put(Key, Value, Properties)).
-define(get_parameter(Key, Properties), maps:get(Key, Properties)).

-define(time_limit(Milliseconds), [{run_time, Milliseconds}]).
-define(agents_limit(N_Agents), [{run_agents, N_Agents}]).
-define(score_limit(Score), [{run_score, Score}]).

-export_type([population_id/0, agent_id/0]).

%%% A population is a group of agents. We create a NN population when we are trying to solve a particular problem, or
%%% wish to apply a function to some simulation. Thus, a population of agents is spawned for a particular purpose. Each
%%% simulation or application requires a specific set of morphologies (sensors, actuators, activation functions...),
%%% specified in the population data structure. The population element would at a high abstraction level dictate what
%%% the given group of agents are allowed to interface with, what type algorithms are used to find the champions and
%%% the growing speed
-type population_id() :: {Population_Reference :: reference(), population}.
-record(population, {
	id :: population_id(),
	limit = 5 :: integer(), % Maximum number of agents running at the same time
	minimum = 4 :: integer(), % Minimum number of agents before trigger evolutionary algorithm
	run_time = infinity :: integer(), % Milliseconds of running time before pausing the population
	run_agents = infinity :: integer(), % Number of Agents of tested before pausing the population
	run_score = infinity :: float(), % Score target before pausing the population
	evo_alg_f = fun evolutionary_algorithm:static_on_limit/2 :: function(),
	sel_alg_f = fun selection_algorithm:statistic_top_3/2 :: function()}).

%%% The agent is the record representation of the agent and the phenotype. It has a global view of a function, and is
%%% used to monitor the processes, to restore if damaged, to score the function effectiveness, and other services.
-type agent_id() :: {Agent_Reference :: reference(), agent}.
-record(agent, {
	id :: agent_id(),
	module :: module(),
	properties :: #{},
	mutation_f :: function(),
	behaviour = gen_server :: module(),
	father = none :: agent_id()}). % Agent_Id of the agent that generated this agent one generation back
