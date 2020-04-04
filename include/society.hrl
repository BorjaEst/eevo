%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-define(EV_POOL, society_pool).
-define(EV_POOL_OPTIONS, [
    named_table,        % The name of the table must be global
    set,                % The pool must be a set (no repeated values)
    {keypos, #population.id}    % The key of the record must be the id
]).

% A population is a group of agents which run a (or in a) simulation..
-record(population, {
    id               :: ruler:id(), % Population identification
    supervisor       :: pid(),      % Population supervisor
    ruler            :: pid(),      % Population ruler
    score_pool       :: ets:tid(),  % Id of the score pool
    size       = 0   :: integer(),  % Population size
    run_time   = 0   :: integer(),  % Time population has run
    generation = 0   :: integer(),  % Number of run agents
    best_score = 0.0 :: float()     % Max achieved score
}).








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




