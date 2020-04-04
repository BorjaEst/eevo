%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(selection).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([func/2]).
-export_type([func/0]).

-type func() :: top3 | ramp3.


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Applies the indicated selection funtion returning an agent id.
%% Note the input ScoreAgents is an ordered list of {Score, Agent_id}.
%% @end
%%--------------------------------------------------------------------
-spec func(Function, ScoreAgents) ->  Agent_Id when 
    Function    :: func(),
    ScoreAgents :: [{Score :: number(), Agent_Id :: agent:id()}],
    Agent_Id    :: agent:id().
func(Function, ScoreAgents) ->
    Result = apply_fun(Function, ScoreAgents),
    Result.

apply_fun( top3, ScoreAgents) ->   top3(ScoreAgents);
apply_fun(ramp3, ScoreAgents) ->  ramp3(ScoreAgents); 
apply_fun( _Ref,_ScoreAgents) ->  error(not_defined).


%%====================================================================
%% Selection functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% % TODO: To make description
%%
%% @end
%%--------------------------------------------------------------------
% TODO: Define specs

top3(ScoreAgents) when length(ScoreAgents) > 2 ->
    Top3 = lists:sublist(ScoreAgents, 3),
    {_, Agent_Id} = ltools:randnth(Top3),
    Agent_Id;
top3([_,_] = ScoreAgents) -> 
    {_, Agent_Id} = ltools:randnth(ScoreAgents),
    Agent_Id;
top3([{_, Agent_Id}]) -> 
    Agent_Id;
top3([]) -> 
    error(badarg).

%%--------------------------------------------------------------------
%% @doc
%% % TODO: To make description
%%
%% @end
%%--------------------------------------------------------------------
% TODO: Define specs
ramp3(ScoreAgents) when length(ScoreAgents) > 2 ->
    Top3 = lists:sublist(ScoreAgents, 3),
    {_, Agent_Id} = random_by_score(Top3),
    Agent_Id;
ramp3([_,_] = ScoreAgents) -> 
    {_, Agent_Id} = random_by_score(ScoreAgents),
    Agent_Id;
ramp3([{_, Agent_Id}]) -> 
    Agent_Id;
ramp3([]) -> 
    error(badarg).


%%====================================================================
%% Internal functions
%%====================================================================
random_by_score(ScoreAgents) -> 
    {Scores, _} = lists:unzip(ScoreAgents),
    SumScores   = lists:sum(Scores),
    random_by_score(ScoreAgents, rand:uniform() * SumScores).

random_by_score([{Score,Id}|   _], Stop) when Score > Stop -> 
    Id;
random_by_score([{Score, _}|SIdx], Stop)                   -> 
    random_by_score(SIdx, Stop - Score).

random_by_score_test() -> 
    ScoreAgents = [{50,a}, {30,b}, {20,c}],
    ?assert(length([X 
         || X <-[random_by_score(ScoreAgents)
                    || _<-lists:seq(1,1000)], 
            X==a
        ]) > 400
    ).


%%====================================================================
%% Eunit white box tests
%%====================================================================

% -------------------------------------------------------------------
% TESTS DESCRIPTIONS ------------------------------------------------


% --------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS -------------------------------------------


% --------------------------------------------------------------------
% ACTUAL TESTS -------------------------------------------------------


% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS --------------------------------------------------------------------------------------------

    



