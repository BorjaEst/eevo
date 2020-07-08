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
    State = #{raise_error => false},
    [Score_base, Sleep_time, State].

mutation_example(Score_base, Sleep_time, State) ->
    case rand:uniform() of
        X when X < 0.10 -> [Score_base, Sleep_time, #{raise_error=>1}];
        X when X < 0.20 -> [Score_base, Sleep_time, #{raise_error=>2}];
        _ -> [Score_base + rand:uniform(10), Sleep_time, State]
    end.

function_example(Base_score, Sleep_time, #{raise_error:=false}) ->
    timer:sleep(Sleep_time),
    Score = rand:uniform(100),
    {stop, normal, [{score, Base_score+Score}]};
function_example(_, Sleep_time, #{raise_error:=Number}) ->
    timer:sleep(Sleep_time),
    error({error_number, Number}).

random_score() -> 
    #{
        mutation  => fun mutation_example/3,
        function  => fun function_example/3,
        arguments => arguments_example()
    }.


% --------------------------------------------------------------------
error_agent() -> 
    #{
        mutation  => fun() -> [] end,
        function  => fun() -> error(test_error) end,
        arguments => []
    }.

