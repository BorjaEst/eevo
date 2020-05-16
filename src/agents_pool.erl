%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(agents_pool).
-compile([export_all, nowarn_export_all]). %% TODO: To delete after buil

%% API
-export([]).
-export_type([tid/0]).

-type tid() :: ets:tid().

% This table contains the identification about the runing agents
-record(dni, {
    pid           :: pid(),      % Identifies the process id
    agent_id      :: agent:id(), % Database agent identification
    population_id :: population:id(), % Population it belongs to
    score_group   :: scorer:group()   % Scorer group where to score
}).
-type info() :: #{'agent_id'      => agent:id(),
                  'population_id' => population_id:id(),
                  'score_group'   => scorer:group()}.

-define(AGENTS_POOL, agents_pool).
-define(TAB_CONFIGUTATION, [
    named_table,        % The name of the table must be global
    public,             % Multiple agents have to register in it
    set,                % The pool must be a set (no repeated values)
    {keypos, #dni.pid}  % The key of the record must be the pid
]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the agents pool ets table.
%% @end
%%--------------------------------------------------------------------
-spec start() -> ok.
start() -> 
    ?AGENTS_POOL = ets:new(?AGENTS_POOL, ?TAB_CONFIGUTATION),
    ok.

%%--------------------------------------------------------------------
%% @doc Registers a new agent in the agents pool.
%% @end
%%--------------------------------------------------------------------
-spec register(Pid, Id, Population_id, Score_Group) -> boolean() when 
    Pid           :: pid(),
    Id            :: agent:id(),
    Population_id :: population:id(),
    Score_Group   :: scorer_group.
register(Pid, Id, Population_id, Score_Group) -> 
    ets:insert(?AGENTS_POOL, #dni{pid           = Pid,      
                                  agent_id      = Id, 
                                  population_id = Population_id,
                                  score_group   = Score_Group}).

%%--------------------------------------------------------------------
%% @doc Deletes an agent from the agents pool.
%% @end
%%--------------------------------------------------------------------
-spec unregister(Pid) -> true when 
    Pid :: pid().
unregister(Pid) -> 
    ets:delete(?AGENTS_POOL, Pid).

%%--------------------------------------------------------------------
%% @doc Returns the agent information.
%% @end
%%--------------------------------------------------------------------
-spec info(Pid)  -> InfoMap when
      Pid     :: pid(),
      InfoMap :: info().
info(Pid) -> 
    case ets:lookup(?AGENTS_POOL, Pid) of 
        []  -> error(badarg);
        [I] -> #{agent_id       => I#dni.agent_id,
                 population_id  => I#dni.population_id,
                 score_group    => I#dni.score_group}
    end.


%%====================================================================
%% Internal functions
%%====================================================================


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


