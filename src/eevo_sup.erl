%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%--------------------------------------------------------------------
-module(eevo_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([new_supervisor/1, stop_supervisor/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(POP_SUP_ID(Pop_Id), {element(2, Pop_Id), pop_sup}).
-define(SPECS_POP_SUP(Population_id), #{
    id       => ?POP_SUP_ID(Population_id),
    start    => {pop_sup, start_link, []},
    restart  => transient,
    type     => supervisor,
    modules  => [supervisor]
}).

%%====================================================================
%% API functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts the supervisor
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @doc Requests the supervisor to start a population supervisor 
%% @end
%%--------------------------------------------------------------------
new_supervisor(Population_id) ->
    Specs = ?SPECS_POP_SUP(Population_id),
    {ok, Pid} = supervisor:start_child(?SERVER, Specs),
    Pid.

%%--------------------------------------------------------------------
%% @doc Requests the supervisor to terminate a population supervisor
%% @end
%%--------------------------------------------------------------------
stop_supervisor(Population_id) ->
    supervisor:terminate_child(?SERVER, ?POP_SUP_ID(Population_id)).


%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupFlags = #{strategy  => one_for_one, 
                 intensity => 10,
                 period    => 36},
    ChildSpecs = [],
    agents_pool:start(),
    {ok, {SupFlags, ChildSpecs}}.

    
%%====================================================================
%% Internal functions
%%====================================================================

