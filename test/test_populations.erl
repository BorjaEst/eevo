%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(test_populations).
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

%% API 
% -export([]).


%%%===================================================================
%%% Defined agents
%%%===================================================================

% --------------------------------------------------------------------
n5_1000ms() -> 
    #{
        max_size  => 5,
        stop_time => 1000
    }.

