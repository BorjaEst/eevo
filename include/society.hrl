%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-define(EV_POOL, society_pool).
-define(EV_POOL_OPTIONS, [
    named_table,        % The name of the table must be global
    public,             % Supervisor and ruler must register inside
    set,                % The pool must be a set (no repeated values)
    {keypos, #population.id}    % The key of the record must be the id
]).

% A population is a group of agents which run a (or in a) simulation.
-record(population, {
    id         :: ruler:id(), % Population identification
    supervisor :: pid(),      % Population supervisor
    ruler      :: pid(),      % Population ruler
    score_pool :: ets:tid(),  % Id of the score pool
    size       :: integer(),  % Population size
    run_time   :: integer(),  % Time population has run
    generation :: integer(),  % Number of run agents
    best_score :: float()     % Max achieved score
}).


-define(add_score(Properties, Score), 
    eevo:add_score(Population_Id, Agent_Id, Score)).
-define(end_agent(Properties), exit(end_agent)).

-define(time_limit(Milliseconds), [{run_time, Milliseconds}]).
-define(agents_limit(N_Agents),   [{run_agents, N_Agents}]).
-define(score_limit(Score),       [{run_score, Score}]).

