%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(agent).
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

-export([start_link/1]). 
-export_type([id/0]).

-type id() :: {Ref :: reference(), agent}.


%%%===================================================================
%%% Defined agents
%%%===================================================================

start_link(Agent_Id) ->
    Agent    = edb:read(Agent_Id),
    Function = demography:function(Agent),
    Arg      = demography:arguments(Agent),
    {ok, spawn_link(fun()-> Function(Arg) end)}.



%%%===================================================================
%%% Internal functions
%%%===================================================================

