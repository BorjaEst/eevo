%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%--------------------------------------------------------------------
-module(eevo_sup).
-behaviour(supervisor).

-include_lib("society.hrl").

%% API
-export([start_link/0, start_population/1, stop_population/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(POP_SUP_ID(Pop_Id), {element(1, Pop_Id), pop_sup}).
-define(SPECS_POP_SUP(Population_Id), #{
    id       => ?POP_SUP_ID(Population_Id),
    start    => {pop_sup, start_link, [Population_Id]},
    restart  => temporary,
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
start_population(Pop_Id) ->
    true = ets:insert(?EV_POOL, #population{id = Pop_Id}),
    {ok, Pop_Sup} = supervisor:start_child(?SERVER, 
                                           ?SPECS_POP_SUP(Pop_Id)),
    {ok, _}       = pop_sup:start_ruler(Pop_Sup, Pop_Id),
    ok. 

%%--------------------------------------------------------------------
%% @doc Requests the supervisor to terminate a population supervisor
%% @end
%%--------------------------------------------------------------------
stop_population(Pop_Id) ->
    case supervisor:terminate_child(?SERVER, ?POP_SUP_ID(Pop_Id)) of
        ok   -> true = ets:delete(?EV_POOL, Pop_Id), ok;
        Fail -> Fail
    end.


%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupFlags = #{strategy  => rest_for_one, 
                 intensity => 10,
                 period    => 36},
    ChildSpecs = [],
    start_ev_pool(),
    start_agents_pool(),
    {ok, {SupFlags, ChildSpecs}}.

    
%%====================================================================
%% Internal functions
%%====================================================================

start_ev_pool() ->  
    ets:new(?EV_POOL, ?EV_POOL_OPTIONS).


start_agents_pool() ->  
    ets:new(?AGENTS_POOL, ?AGENTS_POOL_OPTIONS).

