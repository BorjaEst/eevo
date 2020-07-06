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

-type id()       :: {population, Ref :: reference()}.
-type run_data() :: #{'generation' => integer(),
                      'runtime'    => integer(),
                      'best_score' =>   float()}.
-type info()     :: #{score_table => atom(), 
                      selection   => selection:func(),
                      run_data    => run_data()}.

% A population is a group of agents which run a (or in a) simulation.
-define(ID(Name), {population, Name}).
-record(population, {
    name         :: atom(), % Population identification
    selection    :: selection:func(), % Selection for agents pool
    run_data     :: run_data()
}).
-type population() :: #population{}.


%%%===================================================================
%%% API
%%%===================================================================

%%-------------------------------------------------------------------
%% @doc Record fields of a population record.  
%% @end
%%-------------------------------------------------------------------
-spec record_fields() -> ListOfFields :: [atom()].
record_fields() -> record_info(fields, population).

%%--------------------------------------------------------------------
%% @doc Creates a new population.  
%% @end
%%--------------------------------------------------------------------
-spec new(Name::atom(), Selection::selection:func()) -> Id::id().
new(Name, Selection) ->
    Population = #population{
        name        = Name,
        selection   = Selection,
        run_data    = #{generation =>   0,
                        runtime    =>   0,
                        best_score => 0.0}
    },
    ok  = mnesia:write(Population),
    {population, Name}.

%%--------------------------------------------------------------------
%% @doc Returns the population information (as map).  
%% @end
%%--------------------------------------------------------------------
-spec info(Id::id()) -> Info::info().
info(Id) -> 
    Population = do_read(Id),
    #{
        score_table => Population#population.name,
        selection   => Population#population.selection,
        run_data    => Population#population.run_data
    }.

%%--------------------------------------------------------------------
%% @doc Transforms the id to score table name.  
%% @end
%%--------------------------------------------------------------------
-spec score_table(id()) -> atom().
score_table({population, Name}) -> Name.

%%--------------------------------------------------------------------
%% @doc Updates the parameter run_data.  
%% @end
%%--------------------------------------------------------------------
-spec update(Id::id(), Data::run_data()) -> ok.
update(Id, Data) -> 
    Function = fun(P) -> P#population{run_data=Data} end,
    do_update(Id, Function).


%%%===================================================================
%%% Internal functions
%%%===================================================================

% Reads the population from mnesia ----------------------------------
do_read(Id) -> 
    case mnesia:read(Id) of 
        [Population] -> Population;
         []          -> error(not_found)
    end.

% Updates the population in mnesia ----------------------------------
do_update(Id, Function) -> 
    case mnesia:wread(Id) of 
        [Population] -> ok = mnesia:write(Function(Population));
         []          -> error(not_found)
    end.


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
