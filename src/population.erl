%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(population).
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

%% API
%%-export([start_link/0]).
-export_type([id/0, population/0, run_data/0, info/0]).

-type id()       :: {Ref :: reference(), ruler}.
-type run_data() :: #{'generation' => integer(),
                      'runtime'    => integer(),
                      'best_score' =>   float()}.
-type info()     :: #{score_table => atom(), 
                      selection   => selection:func(),
                      run_data    => run_data()}.

% A population is a group of agents which run a (or in a) simulation.
-define(ID(Name), {Name, population}).
-record(population, {
    id           :: id(),   % Population identification
    score_table  :: atom(), % Name of the score table 
    selection    :: selection:func(), % Selection for agents pool
    run_data     :: run_data()
}).
-type population() :: #population{}.


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new population.  
%% @end
%%--------------------------------------------------------------------
-spec new(Name :: atom(), selection:func()) -> population().
new(Name, Selection) ->
    #population{
        id          = ?ID(Name),
        score_table = Name,
        selection   = Selection,
        run_data    = #{generation =>   0,
                        runtime    =>   0,
                        best_score => 0.0}
    }.

%%--------------------------------------------------------------------
%% @doc Returns the population id.  
%% @end
%%--------------------------------------------------------------------
-spec id(population()) -> id().
id(Population) -> Population#population.id.

%%--------------------------------------------------------------------
%% @doc Transforms the id to score table name.  
%% @end
%%--------------------------------------------------------------------
-spec score_table(id()) -> atom().
score_table({Name, population}) -> Name.

%%-------------------------------------------------------------------
%% @doc Record fields of a population record.  
%% @end
%%-------------------------------------------------------------------
-spec record_fields() -> ListOfFields :: [atom()].
record_fields() -> record_info(fields, population).

%%--------------------------------------------------------------------
%% @doc Returns the population information (as map).  
%% @end
%%--------------------------------------------------------------------
-spec info(population()) -> info().
info(Population) -> 
    #{
        score_table => Population#population.score_table,
        selection   => Population#population.selection,
        run_data    => Population#population.run_data
    }.

%%--------------------------------------------------------------------
%% @doc Updates the parameter run_data.  
%% @end
%%--------------------------------------------------------------------
-spec update(population(), Data :: run_data()) -> population().
update(Population, Data) -> Population#population{run_data = Data}.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%%====================================================================
%% Eunit white box tests
%%====================================================================

% --------------------------------------------------------------------
% TESTS DESCRIPTIONS -------------------------------------------------

% --------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS -------------------------------------------

% --------------------------------------------------------------------
% ACTUAL TESTS -------------------------------------------------------

% --------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS ------------------------------------------
