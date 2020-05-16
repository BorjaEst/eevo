%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pop_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_agent/3, stop_agent/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SPECS_AGENT(Id, SGroup), #{
    id       => Id,
    start    => {agent, start_link, [Id, SGroup]},
    restart  => temporary,
    shutdown => 100,
    modules  => [agent]
 }).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the supervisor
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link(?MODULE, []).

%%--------------------------------------------------------------------
%% @doc Requests the supervisor to start an agent.
%% @end
%%--------------------------------------------------------------------
start_agent(Supervisor, SGroup, Id) ->
    supervisor:start_child(Supervisor, ?SPECS_AGENT(Id, SGroup)).

%%--------------------------------------------------------------------
%% @doc Requests the supervisor to terminate an agent.
%% @end
%%--------------------------------------------------------------------
stop_agent(Supervisor, Id) ->
    supervisor:terminate_child(Supervisor, Id).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupFlags = #{strategy  => one_for_all, %% All down if one down
                 intensity => 0,   %% Restart is not allowed
                 period    => 10}, %% Any as intensity = 0
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================

