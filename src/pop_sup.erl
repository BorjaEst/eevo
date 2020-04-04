%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pop_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_ruler/2, start_agents_supervisor/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SPECS_RULER(Population_Id, ), #{
    id       => Population_Id,
    start    => {ruler, start_link, [??]},
    restart  => permanent,
    shutdown => 1000,
    modules  => [gen_server]
}).
-define(SPECS_AGENTS(Agent_Id), #{
    id       => Agent_Id,
    start    => {agent, start_link, [Agent_Id]},
    restart  => temporary,
    shutdown => 100
 }).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link(?MODULE, []).

%%--------------------------------------------------------------------
%% @doc
%% Requests the supervisor to start a population supervisor
%%
%% @end
%%--------------------------------------------------------------------
start_ruler(Supervisor, Arguments) ->
    Population_Id = maps:get(id, Arguments),
    supervisor:start_child(Supervisor, ?SPECS_RULER(Population_Id, maps:to_list(Arguments))).

%%--------------------------------------------------------------------
%% @doc
%% Requests the supervisor to terminate a population supervisor
%%
%% @end
%%--------------------------------------------------------------------
start_agent(Supervisor, Agent_Id) ->
    supervisor:start_child(Supervisor, ?SPECS_AGENTS(Population_Id)).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupFlags = #{strategy => one_for_all, %% If an element dies, all must shutdown
                 intensity => 0, %% Restart is not allowed
                 period => 10}, %% Any as intensity = 0
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
