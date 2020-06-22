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
    Score_base = rand:uniform(20),
    Sleep_time = rand:uniform(10),
    State = #{key => value},
    [Score_base, Sleep_time, State].

mutation_example(Score_base, Sleep_time, State) ->
    Extra_score = rand:uniform(10),
    [Score_base + Extra_score, Sleep_time, State].

function_example(Base_score, Sleep_time, _State) ->
    timer:sleep(Sleep_time),
    Score = rand:uniform(100),
    {stop, normal, [{score, Base_score+Score}]}.

random_score() -> 
    #{
        mutation  => fun mutation_example/3,
        function  => fun function_example/3,
        arguments => arguments_example()
    }.

