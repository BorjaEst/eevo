%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @TODO: 
%%% @TODO: Change time by energy, agent dies if energy =< 0.0
%%% @TODO: Old scores must be deleted as the context changes, performance?
%%% @TODO: GUI based on web
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(eevo).
-author("borja").
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
% -export([]).
-export_Types([population/0, rules/0, selection/0, info/0]).
-export_Types([agent/0, features/0]).

-type population() :: population:id().
-type agent()      :: agent:id().
-type selection()  :: selection:func().
-type features()   :: agent:features().
-type info()       :: population:info().
-type tree()       :: #{agent() => tree()}.
-type results()    :: #{population := info(),
                        tree       := tree(),
                        top3       := [agent()]}.


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new population in the eevo database
%% @end
%%--------------------------------------------------------------------
-spec population(Name :: atom()) -> population().
population(Name) -> population(Name, top3).

-spec population(Name :: atom(), selection()) -> population().
population(Name, Selection) ->
    Function = fun() -> population:new(Name, Selection) end,
    {atomic, Population} = mnesia:transaction(Function),
    Population.

%%--------------------------------------------------------------------
%% @doc Runs a population starting by the defined seeds until the 
%% condition of stop function returns true. The Stop function should 
%% be arity 1 using the map from run_data from population info.
%% If a name is defined instead a population, a new population is 
%% created (what can overwrite the previous one).
%% @end
%%--------------------------------------------------------------------
-spec run(NameOrPop, Seeds, Size, Stop_condition) -> results() when
    NameOrPop       :: atom() | population(),
    Seeds           :: [agent()],
    Size            :: non_neg_integer(),
    Stop_condition  :: function().
run(Name, Seeds, Size, Stop_condition) when is_atom(Name) ->
    run(population(Name), Seeds, Size, Stop_condition);
run(Id, Seeds, Size, Stop) when is_function(Stop) ->
    ok = ruler:run(Id, Seeds, Size, Stop),
    #{population => info(Id), 
      tree       => tree(Id),
      top3       => top(Id, 3)}.

%%--------------------------------------------------------------------
%% @doc Runs as an agent calling its function with the arguments.
%% @end
%%--------------------------------------------------------------------
-spec run_as(Agent::agent()) -> TBD::term(). %TODO: define term
run_as(Agent_id) -> 
    Get_info = fun() -> agent:features(Agent_id) end,
    {atomic, Info} = mnesia:transaction(Get_info),
    #{function:=Fun, arguments:=Arg} = Info,
    apply(Fun, Arg).

%%--------------------------------------------------------------------
%% @doc Creates a new agent with the indicated features.
%% @end
%%--------------------------------------------------------------------
-spec agent(features()) -> agent().
agent(Features) ->
    {atomic, Agent} = mnesia:transaction(
        fun() -> 
            agent:new(Features) 
        end),
    ?LOG_DEBUG(#{what => "New agent", id => Agent}),
    Agent.

%%--------------------------------------------------------------------
%% @doc Returns the info from a population or agent.
%% @end
%%--------------------------------------------------------------------
-spec info(population() | agent()) -> info() | features().
info({population, _} = Population_id) ->
    Function = fun() -> population:info(Population_id) end,
    {atomic, Info} = mnesia:transaction(Function),
    Info;

info({agent, _} = Agent_id) ->
    Function = fun() -> agent:features(Agent_id) end,
    {atomic, Info} = mnesia:transaction(Function),
    Info.

%%--------------------------------------------------------------------
%% @doc Add a score value to an agent id. The call is asynchronous.
%% @end
%%--------------------------------------------------------------------
-spec score(Agent_Pid::pid(), Points::float()) -> ok.
score(Pid, Points) ->
    #{agent_id    := Agent_id, 
      score_group := Score_group} = agents_pool:info(Pid),
    scorer:add_score(Score_group, Agent_id, Points).

%%--------------------------------------------------------------------
%% @doc Returns the ETS with the score table managed by a ruler.
%% @end
%%--------------------------------------------------------------------
-spec score_table(population()) -> atom().
score_table(Population_id) -> 
    population:score_table(Population_id).

%%--------------------------------------------------------------------
%% @doc Returns the N agents with the highest score.
%% @end
%%--------------------------------------------------------------------
-spec top(population(), N :: integer()) ->
    [{agent(), Score :: float()}].
top(Id, N) ->
    Tab = score_table(Id),
    DirtyPool = {x,Tab,pool}, %scorer:top doesn't use the element 1
    scorer:top(DirtyPool, N).

%%--------------------------------------------------------------------
%% @doc Returns the N agents with the lowest score.
%% @end
%%--------------------------------------------------------------------
-spec bottom(population(), N :: integer()) ->
    [{agent(), Score :: float()}].
bottom(Id, N) ->
    Tab = score_table(Id),
    DirtyPool = {x,Tab,pool}, %scorer:bottom doesn't use the element 1
    scorer:bottom(DirtyPool, N).

%%--------------------------------------------------------------------
%% @doc Returns the genealogical tree of an agent or population.
%% @end
%%--------------------------------------------------------------------
-spec tree(agent() | population()) -> tree:tree().
tree({population, _} = Id) -> 
    Agents_ids = agents_list(Id),
    Get_Tree   = fun() -> tree:from_list(list_tree(Agents_ids)) end,
    {atomic, Tree} = mnesia:transaction(Get_Tree),
    Tree;
tree({agent,_} = Id) -> 
    {atomic, Tree} = mnesia:transaction(fun() -> tree(Id, #{}) end),
    Tree. 

tree(undefined, Tree) -> Tree;
tree( Agent_id, Tree) -> 
    tree(agent:parent(Agent_id), #{Agent_id => Tree}).

%%--------------------------------------------------------------------
%% @doc Reads the father agent, applies the mutation function to the 
%% agent properties, saves the mutated agent on the nndb and returns 
%% its Id.
%% @end
%%--------------------------------------------------------------------
-spec mutate(Agent::agent()) -> Child::agent().
mutate(Agent) ->
    {atomic, Child} = mnesia:transaction(
        fun() ->
            Clone = agent:clone(Agent),
            ok = agent:mutate(Clone),
            Clone
        end),
    ?LOG_INFO(#{what => "Agent mutated", id => Agent, 
                details => #{child => Child}}),
    Child.

%%%===================================================================
%%% Internal functions
%%%===================================================================

% Returls a list of the population agents ids -----------------------
agents_list(Population_id) -> 
    Tab = score_table(Population_id),
    DirtyPool = {x,Tab,pool}, %scorer:to_list doesn't use the element 1
    ScoreIdList = scorer:to_list(DirtyPool),
    {_, Agents_ids} = lists:unzip(ScoreIdList),
    Agents_ids.

% Returls a list of the listed agents as {Child, Parent} ------------
list_tree([Id|Ids]) -> 
   [{Id, agent:parent(Id)} | list_tree(Ids)];
list_tree([]) -> 
    [].

