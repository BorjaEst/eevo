%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%--------------------------------------------------------------------
-module(eevo_sup).
-behaviour(supervisor).

%% API
-export([start_link/1, start_population_supervisor/1, terminate_population_supervisor/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(SPECS_EEVO_SRV(StartArgs), #{
    id       => eevo_srv,
    start    => {eevo_srv, start_link, [StartArgs]},
    restart  => permanent,
    shutdown => 1000,
    modules  => [gen_server]}).

-define(POP_SUP_ID(Population_Id), {element(1, Population_Id), pop_sup}).
-define(SPECS_POP_SUP(Population_Id), #{
    id       => ?POP_SUP_ID(Population_Id),
    start    => {pop_sup, start_link, []},
    restart  => temporary,
    type     => supervisor,
    modules  => [supervisor]}).

%%====================================================================
%% API functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
start_link(StartArgs) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, StartArgs).

%%--------------------------------------------------------------------
%% @doc
%% Requests the supervisor to start a population supervisor
%%
%% @end
%%--------------------------------------------------------------------
start_population_supervisor(Population_Id) ->
    supervisor:start_child(?SERVER, ?SPECS_POP_SUP(Population_Id)).

%%--------------------------------------------------------------------
%% @doc
%% Requests the supervisor to terminate a population supervisor
%%
%% @end
%%--------------------------------------------------------------------
terminate_population_supervisor(Population_Id) ->
    supervisor:terminate_child(?SERVER, ?POP_SUP_ID(Population_Id)).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init(StartArgs) ->
    SupFlags = #{strategy  => rest_for_one, % Avoid populations alive if eevo_srv dies
                 intensity => 10,
                 period    => 36},
    ChildSpecs = [
        ?SPECS_EEVO_SRV(StartArgs)
    ],
    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
