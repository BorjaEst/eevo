%%%-------------------------------------------------------------------
%%% @author borja
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Sep 2018 20:14
%%%-------------------------------------------------------------------

-define(EV_POOL, ev_pool).
-define(EV_POOL_OPTIONS, [
    named_table,        % The name of the table must be global for now
    % public,             % As any process should be able to start a nn

    set,                % The pool must be a set (no repeated values)
    {keypos, #population.id}    % The key of the record must be the id
]).

% A population is a group of agents which run a (or in a) simulation..
-record(population, {
    id             :: ruler:id(),
    supervisor     :: pid(),
    ruler       :: pid(),
    score_table    :: ets:tid(),
    limit      = 5 :: integer(), % Maximum number of agents running at the same time
    minimum    = 4 :: integer(), % Minimum number of agents before trigger evolutionary algorithm
    current    = 0 :: integer(),
    run_time   = infinity :: integer(), % Milliseconds of running time before pausing the population
    run_agents = infinity :: integer(), % Number of Agents of tested before pausing the population
    run_score  = infinity :: float(), % Score target before pausing the population
    evo_alg_f  = fun evolutionary_algorithm:static_on_limit/2 :: function(),
    sel_alg_f  = fun selection_algorithm:statistic_top_3/2 :: function()}).




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




