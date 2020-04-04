%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(agents_sup).

-include_lib("society.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0, start_agent/3, stop_agent/2]).

%% Supervisor callbacks
-export([init/1]).

%%-define(SPECS_GEN_AGENT(Agent_Id, Fun, Properties), #{
%%    id       => Agent_Id,
%%    start    => MFA,
%%    restart  => temporary,
%%    shutdown => 100,
%%    modules  => [gen_agent]}). % TODO: To modify if the behaviour changes

-define(SPECS_GEN_SERVER(Agent_Id, MFA), #{
    id       => Agent_Id,
    start    => MFA,
    restart  => temporary,
    shutdown => 100,
    modules  => [gen_server]}).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the agents supervisor
%%
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link(?MODULE, []).

%%--------------------------------------------------------------------
%% @doc
%% Starts an agent under the agents supervisor
%%
%% @end
%%--------------------------------------------------------------------
start_agent(Supervisor, Population_Id, Agent_Id) ->
    #agent{behaviour  = Behaviour,
           module     = Mod,
           properties = Properties} = nndb:read(Agent_Id),
    Specs = case Behaviour of
                gen_server ->
                    ?SPECS_GEN_SERVER(Agent_Id, {Mod, start_link, [Agent_Id, Population_Id, Properties]});
                gen_statem ->
                    error("agent behaviour *gen_statem not implemented on this version");
                supervisor ->
                    error("agent behaviour *supervisor not implemented on this version");
                _Other ->
                    error("agent behaviour *gen_agent not implemented on this version")
            end,
    supervisor:start_child(Supervisor, Specs).

%%--------------------------------------------------------------------
%% @doc
%% Stops the population supervisor
%%
%% @end
%%--------------------------------------------------------------------
stop_agent(Supervisor, Agent_Id) ->
    supervisor:terminate_child(Supervisor, Agent_Id).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
                       MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
          [ChildSpec :: supervisor:child_spec()]}} |
    ignore |
    {error, Reason :: term()}.
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 10,
                 period => 36},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

