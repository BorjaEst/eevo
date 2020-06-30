%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(selection).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([func/2]).
-export_type([func/0]).

-type func() :: top3 | top10 | ramp3 | ramp10.


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Applies the indicated selection funtion returning an agent id.
%% Note the input ScoreAgents is an ordered list of {Score, Agent_id}.
%% @end
%%--------------------------------------------------------------------
-spec func(Function, ScorePool) ->  {ok, Agent_Id} | empty_pool when 
    Function  :: func(),
    ScorePool :: scorer:pool(),
    Agent_Id  :: agent:id().
func(Function, ScorePool) ->
    Result = apply_fun(Function, ScorePool),
    Result.

apply_fun(  top3, ScorePool) ->  any(scorer:top(ScorePool,  3));
apply_fun( ramp3, ScorePool) -> ramp(scorer:top(ScorePool,  3)); 
apply_fun( top10, ScorePool) ->  any(scorer:top(ScorePool, 10));
apply_fun(ramp10, ScorePool) -> ramp(scorer:top(ScorePool, 10)); 
apply_fun(  _Ref,_ScorePool) -> error(not_defined).


%%====================================================================
%% Selection functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Returns a random agent from the list.
%% @end
%%--------------------------------------------------------------------
any([_,_|_] = ScoreAgents) -> 
    {_, Agent_Id} = ltools:randnth(ScoreAgents),
    {ok, Agent_Id};
any([{_, Agent_Id}]) -> {ok, Agent_Id};
any(             []) -> empty_pool.

%%--------------------------------------------------------------------
%% @doc Returns a random agent, but the probability of each is 
%% proportional to the score.
%% @end
%%--------------------------------------------------------------------
ramp([_,_|_] = ScoreAgents) -> 
    {_, Agent_Id} = random_by_score(ScoreAgents),
    {ok, Agent_Id};
ramp([{_, Agent_Id}]) -> {ok, Agent_Id};
ramp(             []) -> empty_pool.


%%====================================================================
%% Internal functions
%%====================================================================

% Returns a random agent proportionally to score --------------------
random_by_score(ScoreAgents) -> 
    {Scores, _} = lists:unzip(ScoreAgents),
    SumScores   = lists:sum(Scores),
    random_by_score(ScoreAgents, rand:uniform() * SumScores).

random_by_score([{Score,Id}|   _], Stop) when Score > Stop -> 
    {Score,Id};
random_by_score([{Score, _}|SIdx], Stop)                   -> 
    random_by_score(SIdx, Stop - Score).

random_by_score_test() -> 
    ScoreAgents = [{50,a}, {30,b}, {20,c}],
    ?assert(length([X 
         || X <-[random_by_score(ScoreAgents)
                    || _<-lists:seq(1,1000)], 
            X=={50,a}
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

    



