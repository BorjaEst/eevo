%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

% This table contains all the data about the running populations
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
    size       :: {Current :: integer(), Limit :: integer()},
    runtime    :: runtime_specs(),     % Time population has run
    generation :: generation_specs(),  % Number of run agents
    score      :: score_specs()        % Max achieved score
}).
-type runtime_specs()    :: {Current :: integer(), Limit :: integer()}.
-type generation_specs() :: {Current :: integer(), Limit :: integer()}.
-type score_specs()      :: {Current ::   float(), Limit ::   float()}. 

% This table contains the identification about the runing agents
-define(AGENTS_POOL, agents_pool).
-define(AGENTS_POOL_OPTIONS, [
    named_table,        % The name of the table must be global
    public,             % Multiple pop_sup have to register in it
    set,                % The pool must be a set (no repeated values)
    {keypos, #dni.pid}  % The key of the record must be the pid
]).
-record(dni, {
    pid           :: pid(),      % Identifies the process id
    agent_id      :: agent:id(), % Database agent identification
    population_id :: ruler:id()  % Population it belongs to
}).


% Simplification macros
-define(add_score(Pid, Score), eevo:add_score(Pid, Score)).
-define(end_agent(Pid), exit(Pid, normal)).

-define(time_limit(Milliseconds), [{runtime, Milliseconds}]).
-define(agents_limit(N_Agents),   [{run_agents, N_Agents}]).
-define(score_limit(Score),       [{run_score, Score}]).

