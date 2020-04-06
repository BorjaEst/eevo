%%%-------------------------------------------------------------------
%%% File    : example_SUITE.erl
%%% Author  :
%%% Description :
%%%
%%% Created :
%%%-------------------------------------------------------------------
-module(eevo_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("society.hrl").

-define(HEAD, [$- || _ <-lists:seq(1,80)] ++ "\n").
-define(HEAD(Text),  ct:log(?LOW_IMPORTANCE, ?HEAD ++ "~p", [Text])).
-define(END,  [$_ || _ <-lists:seq(1,80)]).
-define(END(Value), ct:log(?LOW_IMPORTANCE, "~p~n" ++ ?END, [Value]),
                    Value).

-define(INFO(Text, Info), ct:log(?LOW_IMPORTANCE, "~p: ~p", [Text, Info])).
-define(ERROR(Error),     ct:pal(?HI_IMPORTANCE, "Error: ~p", [Error])).

-define(PARALLEL_POPULATIONS, 5).
-define(PARALLEL_AGENTS,      6).
-define(PERFORMANCE_MEASURE_TIME_MS, 1000).

-define(TEST_POP_TIME_LIMIT, 1000).
-define(TEST_POP_AGENTS_LIMIT, 20).
-define(TEST_POP_SCORE_LIMIT, 80.00).

-define(SEQ(N), lists:seq(1, N)).

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
suite() ->
    [{timetrap, {seconds, 8}}].

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
        {tests_for_multiple_populations, [parallel, shuffle],
         [simple_population || _ <- ?SEQ(?PARALLEL_POPULATIONS)]
        },
        {tests_with_limits, [],
         [
            test_with_time_limit,
            test_with_agents_limit,
            test_with_score_limit
         ]
        }
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
        {group, tests_for_multiple_populations},
        {group, tests_with_limits},
        simple_population  % GROUPS CANNOT BE DEBUGGED
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

% --------------------------------------------------------------------
simple_population() ->
    [].
simple_population(_Config) ->
    Population = test_populations:n5_infinity(),
    Agents = [test_agents:random_score()||_<-?SEQ(?PARALLEL_AGENTS)],
    ok = test_population(Population, Agents).


% --------------------------------------------------------------------
test_with_time_limit() ->
    [].
test_with_time_limit(_Config) ->
    Population = test_populations:n5_1000ms(),
    Agents = [test_agents:random_score()||_<-?SEQ(?PARALLEL_AGENTS)],
    ok = test_population(Population, Agents).

% --------------------------------------------------------------------
test_with_agents_limit() ->
    [].
test_with_agents_limit(_Config) ->
    Population = test_populations:n5_10generations(),
    Agents = [test_agents:random_score()||_<-?SEQ(?PARALLEL_AGENTS)],
    ok = test_population(Population, Agents).

% --------------------------------------------------------------------
test_with_score_limit() ->
    [].
test_with_score_limit(_Config) ->
    Population = test_populations:n5_1000points(),
    Agents = [test_agents:random_score()||_<-?SEQ(?PARALLEL_AGENTS)],
    ok = test_population(Population, Agents).


% --------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS ------------------------------------------

% ....................................................................
test_population(Population, Agents) ->
    {ok, Population_Id} = correct_population_generation(Population),
    {ok,    Agents_Ids} = correct_agents_generation(Agents),
    ok = correct_start(Population_Id),
    ok = correct_agents_addition(Population_Id, Agents_Ids),
    ok = correct_population_evolution(Population_Id),
    ok = correct_stop(Population_Id),
    ok.

% --------------------------------------------------------------------
correct_population_generation(Population) ->
    ?HEAD("Correct generation of a population ....................."),
    Population_Id = eevo:population(Population),
    ?END({ok, Population_Id}).

% --------------------------------------------------------------------
correct_agents_generation(Agents) ->
    ?HEAD("Correct generation of an agents ........................"),
    Agents_Id = [eevo:agent(Agent) || Agent <- Agents],
    ?END({ok, Agents_Id}).

% --------------------------------------------------------------------
correct_start(Population_Id) ->
    ?HEAD("Correct population start ..............................."),
    Population_Id = eevo:start(Population_Id),
    ?END(ok).

% --------------------------------------------------------------------
correct_agents_addition(Population_Id, Agents_Id) ->
    ?HEAD("Correct addition of multiple agents ...................."),
    [ok = eevo:add(Population_Id, Id) || Id <- Agents_Id],
    timer:sleep(200), ?INFO("Agents ids:", Agents_Id),
    PoolList = ets:tab2list(eevo:score_pool(Population_Id)), 
    ?INFO("Pool list after 200ms:", PoolList),
    L = length([A1 || {{_,A2}}<-PoolList, A1<-Agents_Id, A1==A2]),
    L = length(Agents_Id),
    ?END(ok).

% --------------------------------------------------------------------
correct_population_evolution(Population_Id) ->
    ?HEAD("Correct population evolution ..........................."),
    Top20_1 = eevo:top(Population_Id, 20),
    ?INFO("Top20 after some ms", Top20_1),
    timer:sleep(100),
    Top20_2 = eevo:top(Population_Id, 20),
    ?INFO("Top20 after some ms", Top20_2),
    true = Top20_1 /= Top20_2,
    ?END(ok).

% -------------------------------------------------------------------
correct_stop(Population_Id) ->
    ?HEAD("The population is correctly killed ....................."),
    ok = eevo:stop(Population_Id),
    ?END(ok).






