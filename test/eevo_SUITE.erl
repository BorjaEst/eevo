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
-include_lib("stop_conditions.hrl").

-define(HEAD, [$- || _ <-lists:seq(1,80)] ++ "\n").
-define(HEAD(Text),  ct:log(?LOW_IMPORTANCE, ?HEAD ++ "~p", [Text])).
-define(END,  [$_ || _ <-lists:seq(1,80)]).
-define(END(Value), ct:log(?LOW_IMPORTANCE, "~p~n" ++ ?END, [Value]),
                    Value).

-define(INFO(Text, Info), ct:log(?LOW_IMPORTANCE, "~p: ~p", [Text, Info])).
-define(ERROR(Error),     ct:pal(?HI_IMPORTANCE, "Error: ~p", [Error])).

-define(PARALLEL_POPULATIONS, 8).
-define(PARALLEL_AGENTS,      8).
-define(PERFORMANCE_MEASURE_TIME_MS, 1000).

-define(TEST_POP_TIME_LIMIT, 60).
-define(TEST_POP_AGENTS_LIMIT, 40).
-define(TEST_POP_SCORE_LIMIT, 120.00).

-define(SEQ(N), lists:seq(1, N)).
-define(SELECTIONS, [top3, ramp3]).


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
    ok = application:start(mnesia),
    ok = application:start(eevo),
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok = application:stop(mnesia),
    ok = application:stop(eevo),
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
        {tests_with_limits, [],
         [
            test_with_time_limit,
            test_with_agents_limit,
            test_with_score_limit
         ]
        },
        {tests_for_multiple_populations, [parallel, shuffle],
         [simple_population || _ <- ?SEQ(?PARALLEL_POPULATIONS)]
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
        {group, tests_with_limits},
        {group, tests_for_multiple_populations}
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
simple_population(_Config) ->
    AgentsF = [test_agents:random_score()||_<-?SEQ(?PARALLEL_AGENTS)],
    Stop_condition = ltools:randnth(
        [
            ?stop_time(3000),
            ?max_generations(4500),
            ?score_target(1500.0)
        ]
    ),
    {_Id, _Results} = test_population(AgentsF, Stop_condition),
    ok.

% --------------------------------------------------------------------
test_with_time_limit(_Config) ->
    AgentsF = [test_agents:random_score()||_<-?SEQ(?PARALLEL_AGENTS)],
    Stop_condition = ?stop_time(?TEST_POP_TIME_LIMIT),
    {Id, Results} = test_population(AgentsF, Stop_condition),
    print_results({Id, Results}).

% --------------------------------------------------------------------
test_with_agents_limit(_Config) ->
    AgentsF = [test_agents:random_score()||_<-?SEQ(?PARALLEL_AGENTS)],
    Stop_condition = ?max_generations(?TEST_POP_AGENTS_LIMIT),
    {Id, Results} = test_population(AgentsF, Stop_condition),
    print_results({Id, Results}).

% --------------------------------------------------------------------
test_with_score_limit(_Config) ->
    AgentsF = [test_agents:random_score()||_<-?SEQ(?PARALLEL_AGENTS)],
    Stop_condition = ?score_target(?TEST_POP_SCORE_LIMIT),
    {Id, Results} = test_population(AgentsF, Stop_condition),
    print_results({Id, Results}).


% --------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS ------------------------------------------

% Creates a random atom name to test tables and populations ---------
rand_name() -> 
    N = erlang:unique_integer([monotonic, positive]),
    list_to_atom("Pop_" ++ integer_to_list(N)).

% --------------------------------------------------------------------
test_population(Agents_features, Stop_condition) ->
    {ok,      Id} = correct_population_generation(),
    {ok,  Agents} = correct_agents_generation(Agents_features),
    {ok, Results} = correct_run(Id, Agents, Stop_condition),
    {Id, Results}.

% --------------------------------------------------------------------
correct_population_generation() ->
    ?HEAD("Correct generation of a population ....................."),
    Selection = ltools:randnth(?SELECTIONS),
    ?INFO("Selection algorithm to use: ", Selection),
    Population_id = eevo:population(rand_name(), Selection),
    ?END({ok, Population_id}).

% --------------------------------------------------------------------
correct_agents_generation(AgentsF) ->
    ?HEAD("Correct generation of an agents ........................"),
    Agents_Id = [eevo:agent(Features) || Features <- AgentsF],
    ?END({ok, Agents_Id}).

% --------------------------------------------------------------------
correct_run(Id, Agents, Stop_condition) ->
    ?HEAD("Correct population evolution ..........................."),
    Results = eevo:run(Id, Agents, ?PARALLEL_AGENTS +2, Stop_condition),
    ?INFO("Results: ", Results),
    Top5 = eevo:top(Id, 5),
    ?INFO("Top5 after population run", Top5),
    [_|_] = Top5, 
    ?END({ok, Results}).

% -------------------------------------------------------------------
print_results({Id, Results}) -> 
    Data = map_get(population, Results),
    Top3 = map_get(      top3, Results),
    Tree = map_get(      tree, Results),
    ct:print([
        "Training report: \n",
        io_lib:format(" -Id:  \t~p\n", [Id]),
        io_lib:format(" -Data:\t~p\n", [Data]),
        io_lib:format(" -Top3:\t~p\n", [Top3]),
        io_lib:format(" -Tree:\t~p\n", [Tree])
    ]). 

