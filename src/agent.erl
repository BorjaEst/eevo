%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(agent).
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

-export([start_link/1]). 
-export_type([id/0, property/0, properties/0]).

-type id() :: {Ref :: reference(), agent}.
-type property()   :: id | function | arguments | mutation | father.
-type properties() :: #{
    OptionalProperty :: property() => Value :: term()
}.


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new agent and stores it on the database.  
%% @end
%%--------------------------------------------------------------------
-spec new(Properties) -> id() when
    Properties :: properties().
new(Properties) ->
    Agent = demography:agent(Properties),
    edb:write(Agent),
    demography:id(Agent).

%%--------------------------------------------------------------------
%% @doc Spawn function for the population supervisor.
%% @end
%%--------------------------------------------------------------------
-spec start_link(Agent_Id :: id()) -> {ok, Pid :: pid()}.
start_link(Agent_Id) ->
    Agent    = edb:read(Agent_Id),
    Function = demography:function(Agent),
    Arg      = demography:arguments(Agent),
    {ok, spawn_link(fun()-> Function(Arg) end)}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

