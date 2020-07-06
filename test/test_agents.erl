%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(test_agents).
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

%% API 
% -export([]).


%%%===================================================================
%%% Defined agents
%%%===================================================================

% --------------------------------------------------------------------
arguments_example() ->
    [_Score_base=rand:uniform(20)].

mutation_example(Score_base) ->
    [Score_base + rand:uniform(10)].

function_example(Score_base) ->
    timer:sleep(rand:uniform(10)),
    MyScore = rand:uniform(100),
    {stop, normal, [{score, Score_base+MyScore}]}.

random_score() -> 
    #{
        function  => fun function_example/1,
        arguments => arguments_example(),
        mutation  => fun mutation_example/1
    }.

