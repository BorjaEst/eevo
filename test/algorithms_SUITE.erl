%%%-------------------------------------------------------------------
%%% File    : example_SUITE.erl
%%% Author  :
%%% Description :
%%%
%%% Created :
%%%-------------------------------------------------------------------
-module(algorithms_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("society.hrl").

-define(INFO(Info), ct:log(?LOW_IMPORTANCE, "Info report: ~p", [Info])).
-define(ERROR(Error), ct:pal(?HI_IMPORTANCE, "Error report: ~p", [Error])).
-define(EVOLUTION_TRIALS, 20).
-define(AGENTS_TO_GENERATE_ON_TRIAL, 14).
-define(CLONES_TO_GENERATE_ON_TRIAL, 10).

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
suite() ->
    [{timetrap, {seconds, 5}}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    application:start(eevo),
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    application:stop(eevo),
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_testcase(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%%--------------------------------------------------------------------
groups() ->
    [
        {evolutionary_algorithms, [parallel],
         [
             test_for_static_on_limit,
             test_for_uniform_distribution,
             test_for_normal_distribution
         ]},
        {selection_algorithms, [parallel],
         [
             test_for_statistic_ramp,
             test_for_statistic_top3
         ]}
    ].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%%--------------------------------------------------------------------
all() ->
    [
        {group, evolutionary_algorithms},
        {group, selection_algorithms}
    ].

%%--------------------------------------------------------------------
%% Function: TestCase() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
my_test_case_example() ->
    [].

%%--------------------------------------------------------------------
%% Function: TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%%--------------------------------------------------------------------
my_test_case_example(_Config) ->
    ok.

% --------------------------------------------------------------------
% TESTS --------------------------------------------------------------

% --------------------------------------------------------------------..................................................
test_for_static_on_limit() ->
    [].
test_for_static_on_limit(_Config) ->
    Algorithm = fun evolutionary_algorithm:static_on_limit/2,
    display_example_evolutionary_algorithm(Algorithm),
    Options = [{evo_alg_f, Algorithm}],
    Population_Id = eevo:population(Options),
    run_population(Population_Id),
    ok.

% --------------------------------------------------------------------..................................................
test_for_uniform_distribution() ->
    [].
test_for_uniform_distribution(_Config) ->
    Algorithm = fun evolutionary_algorithm:uniform_distribution/2,
    display_example_evolutionary_algorithm(Algorithm),
    Options = [{evo_alg_f, Algorithm}],
    Population_Id = eevo:population(Options),
    run_population(Population_Id),
    ok.

% --------------------------------------------------------------------..................................................
test_for_normal_distribution() ->
    [].
test_for_normal_distribution(_Config) ->
    Algorithm = fun evolutionary_algorithm:normal_distribution/2,
    display_example_evolutionary_algorithm(Algorithm),
    Options = [{evo_alg_f, Algorithm}],
    Population_Id = eevo:population(Options),
    run_population(Population_Id),
    ok.

% --------------------------------------------------------------------..................................................
test_for_statistic_ramp() ->
    [].
test_for_statistic_ramp(_Config) ->
    Algorithm = fun selection_algorithm:statistic_ramp/2,
    display_example_selection_algorithm(Algorithm),
    Options = [{sel_alg_f, Algorithm}],
    Population_Id = eevo:population(Options),
    run_population(Population_Id),
    ok.

% --------------------------------------------------------------------..................................................
test_for_statistic_top3() ->
    [].
test_for_statistic_top3(_Config) ->
    Algorithm = fun selection_algorithm:statistic_top_3/2,
    display_example_selection_algorithm(Algorithm),
    Options = [{sel_alg_f, Algorithm}],
    Population_Id = eevo:population(Options),
    run_population(Population_Id),
    ok.

% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS --------------------------------------------------------------------------------------------

% --------------------------------------------------------------------..................................................
test_list_of_score_agents(N) ->
    Unsorted_ScoreAgents = [{X * X, list_to_atom("agent_" ++ integer_to_list(X))} || X <- lists:seq(1, N)],
    lists:reverse(lists:sort(Unsorted_ScoreAgents)).

% --------------------------------------------------------------------..................................................
display_example_selection_algorithm(Selection_algorithm) ->
    ?INFO("Correct application of the selection algorithm to a trial sample ....................."),
    ScoreAgents_List = test_list_of_score_agents(?AGENTS_TO_GENERATE_ON_TRIAL),
    ?INFO({?AGENTS_TO_GENERATE_ON_TRIAL, ScoreAgents_List}),
    Selected = Selection_algorithm(ScoreAgents_List, ?CLONES_TO_GENERATE_ON_TRIAL),
    ?INFO({?CLONES_TO_GENERATE_ON_TRIAL, Selected}),
    ?INFO("____________________________________________________________________________________OK"),
    ok.

% --------------------------------------------------------------------..................................................
display_example_evolutionary_algorithm(Evolutionary_algorithm) ->
    ?INFO("Correct application of the evolutionary algorithm to a trial sample .................."),
    Current_pop = rand:uniform(10) + 4,
    Pop_Limit = Current_pop + ?CLONES_TO_GENERATE_ON_TRIAL,
    ExamplesOfNClones = [Evolutionary_algorithm(Pop_Limit, Current_pop) || _ <- lists:seq(1, ?EVOLUTION_TRIALS)],
    ?INFO({?CLONES_TO_GENERATE_ON_TRIAL, ExamplesOfNClones}),
    ?INFO("____________________________________________________________________________________OK"),
    ok.

% --------------------------------------------------------------------..................................................
run_population(Population_Id) ->
    ?INFO("Correct agents generation and population start ......................................."),
    TestAgents_Id = [                                    eevo:agent(
        _Module = test_agent, % Module where the gen_agent is implemented
        _AgentProperties = test_agent:properties_example(),
        _MutationF = fun test_agent:mutationF_example/1) || _ <- lists:seq(1, 6)],
    {ok, Gov_PId} = eevo:start(Population_Id),
    timer:sleep(10),
    true = is_process_alive(Gov_PId),
    ?INFO("____________________________________________________________________________________OK"),
    
    ?INFO("Correct addition of multiple overloading agents into the population .................."),
    [eevo:add_agent(Population_Id, Agent_Id) || Agent_Id <- TestAgents_Id],
    timer:sleep(1000),
    true = is_process_alive(Gov_PId),
    eevo:stop(Population_Id),
    false = is_process_alive(Gov_PId),
    ?INFO("____________________________________________________________________________________OK"),
    ok.


