%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(evolutionary_algorithm).
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

-include_lib("eunit/include/eunit.hrl").

%% API
%%-export([]).

%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% % TODO: To make description
%%
%% @end
%%--------------------------------------------------------------------
% TODO: Define specs
null_evolution(_Population_Limit, _Current_population) ->
    _ClonesToGenerate = 0.

%%--------------------------------------------------------------------
%% @doc
%% % TODO: To make description
%%
%% @end
%%--------------------------------------------------------------------
% TODO: Define specs
static_on_limit(Population_Limit, Current_population) when Population_Limit > Current_population ->
    _ClonesToGenerate = Population_Limit - Current_population;
static_on_limit(_Population_Limit, _Current_population) ->
    _ClonesToGenerate = 0.

%%--------------------------------------------------------------------
%% @doc
%% % TODO: To make description
%%
%% @end
%%--------------------------------------------------------------------
% TODO: Define specs
uniform_distribution(Population_Limit, Current_population) when Population_Limit > Current_population ->
    _ClonesToGenerate = rand:uniform(Population_Limit - Current_population);
uniform_distribution(_Population_Limit, _Current_population) ->
    _ClonesToGenerate = 0.

%%--------------------------------------------------------------------
%% @doc
%% % TODO: To make description
%%
%% @end
%%--------------------------------------------------------------------
% TODO: Define specs
normal_distribution(Population_Limit, Current_population) when Population_Limit > Current_population ->
    Range = Average = (Population_Limit - Current_population)/2,
    ClonesToGenerate = round(rand:normal(Average, Range)),
    Expected_population = ClonesToGenerate + Current_population,
    if
        Expected_population > Population_Limit -> Population_Limit - Current_population;
        Expected_population < Current_population -> 0;
        true -> ClonesToGenerate
    end;
normal_distribution(_Population_Limit, _Current_population) ->
    _ClonesToGenerate = 0.

%%====================================================================
%% Internal functions
%%====================================================================


%%====================================================================
%% Eunit white box tests
%%====================================================================

% ----------------------------------------------------------------------------------------------------------------------
% TESTS DESCRIPTIONS ---------------------------------------------------------------------------------------------------
this_example_test_() ->
    % {setup, Where, Setup, Cleanup, Tests | Instantiator}
    [
        {"Test example",
         {setup, local, fun no_setup/0, fun no_cleanup/1, fun test_example/1}}
    ].

% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS ---------------------------------------------------------------------------------------------
no_setup() ->
    ok.

no_cleanup(_) ->
    ok.

% ----------------------------------------------------------------------------------------------------------------------
% ACTUAL TESTS ---------------------------------------------------------------------------------------------------------
test_example(_) ->
    [
        ?_assertEqual(true, true)
    ].


% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS --------------------------------------------------------------------------------------------



    
    



