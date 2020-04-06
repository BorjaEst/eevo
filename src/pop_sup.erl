%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pop_sup).
-behaviour(supervisor).

-include_lib("society.hrl").

%% API
-export([start_link/1, start_ruler/2, start_agent/3, stop_agent/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SPECS_RULER(Ruler_Id), #{
    id       => Ruler_Id,
    start    => {ruler, start_link, [Ruler_Id]},
    restart  => transient,
    shutdown => 1000,
    modules  => [gen_server]
}).
-define(SPECS_AGENT(Agent_Id), #{
    id       => Agent_Id,
    start    => {agent, start_link, [Agent_Id]},
    restart  => temporary,
    shutdown => 100,
    modules  => [agent]
 }).

-define(ETS_TABLE_SPECS, [public]).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the supervisor
%% @end
%%--------------------------------------------------------------------
start_link(Ruler_Id) ->
    supervisor:start_link(?MODULE, [Ruler_Id]).

%%--------------------------------------------------------------------
%% @doc Requests the supervisor to start a population ruler.
%% @end
%%--------------------------------------------------------------------
start_ruler(Supervisor, Ruler_id) ->
    supervisor:start_child(Supervisor, ?SPECS_RULER(Ruler_id)).

%%--------------------------------------------------------------------
%% @doc Requests the supervisor to start an agent.
%% @end
%%--------------------------------------------------------------------
start_agent(Id, Population_Id, Supervisor) ->
    {ok, Pid} = supervisor:start_child(Supervisor, ?SPECS_AGENT(Id)),
    true = ets:insert(?AGENTS_POOL, #dni{
        pid = Pid, agent_id = Id, 
        population_id = Population_Id}),
    {ok, Pid}.

%%--------------------------------------------------------------------
%% @doc Requests the supervisor to terminate an agent.
%% @end
%%--------------------------------------------------------------------
stop_agent(Id, Supervisor) ->
    supervisor:terminate_child(Supervisor, Id).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([Population_Id]) ->
    SupFlags = #{strategy  => one_for_all, %% All down if one down
                 intensity => 0,   %% Restart is not allowed
                 period    => 10}, %% Any as intensity = 0
    ChildSpecs = [],
    register_in_pool(Population_Id),
    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================

register_in_pool(Population_Id) -> 
    true = ets:update_element(?EV_POOL, Population_Id, [
        {#population.supervisor, self()}
    ]).

