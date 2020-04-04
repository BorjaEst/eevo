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

-define(INFO(Info), ct:log(default, ?LOW_IMPORTANCE, "Info report: ~p", [Info])).
-define(ERROR(Error), ct:pal(default, ?HI_IMPORTANCE, "Error report: ~p", [Error])).

-define(PARALLEL_POPULATIONS, 5).
-define(PARALLEL_AGENTS, 6).
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
        {tests_for_multiple_populations, [parallel, shuffle],
         [single_population_and_run || _ <- ?SEQ(?PARALLEL_POPULATIONS)]},
        {tests_with_limits, [],
         [test_with_time_limit,
          test_with_agents_limit,
          test_with_score_limit]}
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
        single_population_and_run,  % GROUPS CANNOT BE DEBUGGED
        {group, tests_for_multiple_populations},
        {group, tests_with_limits}
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
single_population_and_run() ->
    [].
single_population_and_run(_Config) ->
    {ok, Population_Id} = correct_population_generation(),
    {ok, Agents_Ids} = correct_agents_generation(?PARALLEL_AGENTS),
    {ok, _Gov_PId} = correct_start(Population_Id),
    ok = correct_agents_addition(Population_Id, Agents_Ids),
    ok = correct_population_evolution(Population_Id),
    ok = correct_stop(Population_Id),
    ok.

% --------------------------------------------------------------------..................................................
test_with_time_limit() ->
    [].
test_with_time_limit(_Config) ->
    ?INFO("Correct creation of population with time limit ......................................"),
    Test_Agent_Id = eevo:agent(
        _Module = test_agent, % Module where the gen_agent is implemented
        _AgentProperties = test_agent:properties_example(),
        _MutationF = fun test_agent:mutationF_example/1),
    Population_Id = eevo:population(?time_limit(?TEST_POP_TIME_LIMIT)),
    {ok, Gov_PId} = eevo:start(Population_Id),
    ok = eevo:add_agent(Population_Id, Test_Agent_Id),
    timer:sleep(?TEST_POP_TIME_LIMIT),
    PopRun_Result = receive {run_end, Population_Id, Result} -> Result after 10 -> error(my_timeout) end,
    timer:sleep(40),
    false = is_process_alive(Gov_PId),
    ?INFO(PopRun_Result),
    ?INFO("____________________________________________________________________________________OK"),
    ok.

% --------------------------------------------------------------------..................................................
test_with_agents_limit() ->
    [].
test_with_agents_limit(_Config) ->
    ?INFO("Correct creation of population with agents limit ......................................"),
    Test_Agent_Id = eevo:agent(
        _Module = test_agent, % Module where the gen_agent is implemented
        _AgentProperties = test_agent:properties_example(),
        _MutationF = fun test_agent:mutationF_example/1),
    Population_Id = eevo:population(?agents_limit(?TEST_POP_AGENTS_LIMIT)),
    {ok, Gov_PId} = eevo:start(Population_Id),
    ok = eevo:add_agent(Population_Id, Test_Agent_Id),
    timer:sleep(1000),
    PopRun_Result = receive {run_end, Population_Id, Result} -> Result after 10 -> error(my_timeout) end,
    timer:sleep(40),
    false = is_process_alive(Gov_PId),
    ?INFO(PopRun_Result),
    ?INFO("____________________________________________________________________________________OK"),
    ok.

% --------------------------------------------------------------------..................................................
test_with_score_limit() ->
    [].
test_with_score_limit(_Config) ->
    ?INFO("Correct creation of population with score limit ......................................"),
    Test_Agent_Id = eevo:agent(
        _Module = test_agent, % Module where the gen_agent is implemented
        _AgentProperties = test_agent:properties_example(),
        _MutationF = fun test_agent:mutationF_example/1),
    Population_Id = eevo:population(?score_limit(?TEST_POP_SCORE_LIMIT)),
    {ok, Gov_PId} = eevo:start(Population_Id),
    ok = eevo:add_agent(Population_Id, Test_Agent_Id),
    timer:sleep(1000),
    PopRun_Result = receive {run_end, Population_Id, Result} -> Result after 10 -> error(my_timeout) end,
    timer:sleep(40),
    false = is_process_alive(Gov_PId),
    ?INFO(PopRun_Result),
    ?INFO("____________________________________________________________________________________OK"),
    ok.


% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS --------------------------------------------------------------------------------------------

% --------------------------------------------------------------------..................................................
correct_population_generation() ->
    ?INFO("Correct generation of a population ..................................................."),
    Population_Id = eevo:population(),
    Population = nndb:read(Population_Id),
    true = is_record(Population, population),
    ?INFO("____________________________________________________________________________________OK"),
    {ok, Population_Id}.

% --------------------------------------------------------------------..................................................
correct_agents_generation(N) ->
    ?INFO("Correct generation of an agents ......................................................"),
    Agents_Id = [eevo:agent(
        _Module = test_agent,
        _AgentProperties = test_agent:properties_example(),
        _MutationF = fun test_agent:mutationF_example/1)
                 || _ <- ?SEQ(N)],
    Agents = nndb:read(Agents_Id),
    [true = is_record(Agent, agent) || Agent <- Agents],
    ?INFO("____________________________________________________________________________________OK"),
    {ok, Agents_Id}.

% --------------------------------------------------------------------..................................................
correct_start(Population_Id) ->
    ?INFO("Correct start of a population form a defined demography .............................."),
    {ok, Gov_PId} = eevo:start(Population_Id),
    timer:sleep(10),
    true = Gov_PId == eevo:governor(Population_Id),
    true = is_process_alive(Gov_PId),
    ?INFO("____________________________________________________________________________________OK"),
    {ok, Gov_PId}.

% --------------------------------------------------------------------..................................................
correct_agents_addition(Population_Id, TestAgents_Id) ->
    ?INFO("Correct addition of multiple agents into the population .............................."),
    {Async_Agents, [Last]} = lists:split(length(TestAgents_Id) - 1, TestAgents_Id),
    [ok = eevo:add_agent(Population_Id, Agent_Id) || Agent_Id <- Async_Agents], % Asynchronous addition
    {ok, Last_Agent_PId} = eevo:start_agent(Population_Id, Last), % Synchronous start
    true = is_process_alive(Last_Agent_PId),
    timer:sleep(100), ?INFO(TestAgents_Id),
    true = is_process_alive(eevo:governor(Population_Id)),
    PoolList = ets:tab2list(eevo:get_score_pool(Population_Id)), ?INFO(PoolList),
    L = length([Score || {{Score, A2}, _} <- PoolList, A1 <- TestAgents_Id, A1 == A2]), ?INFO(L),
    L = length(TestAgents_Id),
    ?INFO("____________________________________________________________________________________OK"),
    ok.

% --------------------------------------------------------------------..................................................
correct_population_evolution(Population_Id) ->
    ?INFO("Correct population evolution ........................................................."),
    Top5_1 = eevo:get_top(Population_Id, 5),
    timer:sleep(400),
    Top5_2 = eevo:get_top(Population_Id, 5),
    ?INFO(Top5_1),
    ?INFO(Top5_2),
    true = Top5_1 /= Top5_2,
    ?INFO("____________________________________________________________________________________OK"),
    ok.

% --------------------------------------------------------------------..................................................
correct_stop(Population_Id) ->
    ?INFO("The population is correctly killed ..................................................."),
    Governor_PId = eevo:governor(Population_Id),
    true = is_process_alive(Governor_PId),
    ok = eevo:stop(Population_Id),
    false = is_process_alive(Governor_PId),
    ?INFO("____________________________________________________________________________________OK"),
    ok.






