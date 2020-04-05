%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(test_agent).
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

%% API 
% -export([]).


%%%===================================================================
%%% Defined agents
%%%===================================================================

% --------------------------------------------------------------------
arguments_example() ->
    #{score_base => rand:uniform(100)}.

mutation_example(Arguments) ->
    #{score_base := Base} = Arguments,
    Arguments#{score_base := Base + rand:uniform(10)}.

function_example(Arguments) ->
    timer:sleep(rand:uniform(100)),
    MyBase  = maps:get(score_base, Arguments),
    MyScore = rand:uniform(1000),
    eevo:score(self(), MyBase+MyScore).

random_score() -> 
    #{
        function  => fun function_example/1,
        arguments => arguments_example(),
        mutation  => fun mutation_example/1
    }.

