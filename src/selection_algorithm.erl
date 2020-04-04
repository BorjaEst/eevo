%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(selection_algorithm).
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
statistic_ramp(_, 0) ->
	[];
statistic_ramp(ScoreAgents_List, ClonesToGenerate) ->
	_ListOfClones = score_selection_fun(ScoreAgents_List, ClonesToGenerate).
%%--------------------------------------------------------------------
%% @doc
%% % TODO: To make description
%%
%% @end
%%--------------------------------------------------------------------
% TODO: Define specs
statistic_top_3(_, 0) ->
	[];
statistic_top_3(ScoreAgents_List, ClonesToGenerate) ->
	Top3 = lists:sublist(ScoreAgents_List, 3),
	_ListOfClones = score_selection_fun(Top3, ClonesToGenerate).

%%====================================================================
%% Internal functions
%%====================================================================

% ......................................................................................................................
score_selection_fun(ScoreAgents_List, ClonesToGenerate) ->
	RangeId_List = range_id_list(ScoreAgents_List),
	[select_range_id(RangeId_List, rand:uniform()) || _ <- lists:seq(1, ClonesToGenerate)].

% ......................................................................................................................
range_id_list(ScoreAgents_List) ->
	Sorted_AS_List = lists:reverse(lists:sort(ScoreAgents_List)),  % Defensive programming in this application
	TotScore = lists:sum([Score || {Score, _} <- Sorted_AS_List]),
	{RangeId_List, _} = lists:mapfoldl(fun create_range/2, {_Acc0 = 0.0, TotScore}, Sorted_AS_List),
	RangeId_List.

create_range({Score, Agent_Id}, {AccIn, TotScore}) ->
	AccOut = AccIn + Score / TotScore,
	{{AccOut, Agent_Id}, {AccOut, TotScore}}.

% ......................................................................................................................
select_range_id([{N, Id} | _], V) when N >= V -> Id;
select_range_id([_Elem | Rest], V)            -> select_range_id(Rest, V).


%%====================================================================
%% Eunit white box tests
%%====================================================================

% ----------------------------------------------------------------------------------------------------------------------
% TESTS DESCRIPTIONS ---------------------------------------------------------------------------------------------------
statistic_function_test_() ->
% {setup, Where, Setup, Cleanup, Tests | Instantiator}
	[
		{"Correct selection of Id using a range function for probability selection",
		 {setup, local, fun no_setup/0, fun no_cleanup/1, fun test_select_range_id/1}},
		{"Correct generation of a range Id list from a list of {Score, Agent_Id}",
		 {setup, local, fun no_setup/0, fun no_cleanup/1, fun test_range_id_list/1}}
	].

% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS ---------------------------------------------------------------------------------------------
no_setup() ->
	ok.

no_cleanup(_) ->
	ok.

% ----------------------------------------------------------------------------------------------------------------------
% ACTUAL TESTS ---------------------------------------------------------------------------------------------------------

test_range_id_list(_) ->
	ScoreAgents_List_1 = [{50, a}, {30, b}, {20, c}],
	ScoreAgents_List_2 = [{30, b}, {50, a}, {20, c}],  % Defensive programming
	ScoreAgents_List_3 = [{50, a}],
	[
		?_assertEqual([{0.5, a}, {0.8, b}, {1.0, c}], range_id_list(ScoreAgents_List_1)),
		?_assertEqual([{0.5, a}, {0.8, b}, {1.0, c}], range_id_list(ScoreAgents_List_2)), % Defensive programming
		?_assertEqual([{1.0, a}], range_id_list(ScoreAgents_List_3))
	].

test_select_range_id(_) ->
	RangeId_List = [{0.0, a}, {0.2, b}, {0.4, c}, {0.6, d}, {0.8, e}, {1.0, f}],
	[
		?_assertEqual(a, select_range_id(RangeId_List, - 0.1)),
		?_assertEqual(b, select_range_id(RangeId_List, + 0.1)),
		?_assertEqual(c, select_range_id(RangeId_List, + 0.3)),
		?_assertEqual(d, select_range_id(RangeId_List, + 0.5)),
		?_assertEqual(e, select_range_id(RangeId_List, + 0.7)),
		?_assertEqual(f, select_range_id(RangeId_List, + 0.9)),
		?_assertEqual(f, select_range_id(RangeId_List, + 1.0)),
		?_assertError(function_clause, select_range_id(RangeId_List, + 1.1)),
		?_assertError(function_clause, select_range_id([], 0.5))
	].


% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS --------------------------------------------------------------------------------------------

	



