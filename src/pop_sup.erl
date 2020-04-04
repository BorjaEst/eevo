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

-define(RULER_ID(Population_Id), {element(1, Population_Id), ruler}).
-define(SPECS_RULER(Population_Id, Arguments), #{
    id       => ?RULER_ID(Population_Id),
    start    => {ruler, start_link, [Arguments]},
    restart  => permanent,
    shutdown => 1000,
    modules  => [gen_server]}).

-define(AGENTS_SUP_ID(Population_Id), {element(2, Population_Id), agents_sup}).
-define(SPECS_AGENTS_SUP(Population_Id), #{
    id       => ?AGENTS_SUP_ID(Population_Id),
    start    => {agents_sup, start_link, []},
    restart  => permanent,
    type     => supervisor,
    modules  => [supervisor]}).

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
start_agents_supervisor(Supervisor, Population_Id) ->
    supervisor:start_child(Supervisor, ?SPECS_AGENTS_SUP(Population_Id)).

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
