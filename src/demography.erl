%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(demography).
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

-include_lib("eunit/include/eunit.hrl").
-include_lib("society.hrl").

%% API
%%-export([]).
-export_type([type/0, ruler/0, agent/0]).

%%% Populations are composed mostly by 2 types:
-type type()   ::      ruler |      agent.
-type id()     :: ruler:id() | agent:id().

-define(NEW_RULER_ID,       {make_ref(), ruler}).
-define(NEW_AGENT_ID,       {make_ref(), agent}).
-define(REFERENCE(Id), element(2,element(1,Id))).

-record(ruler, {
    id = ?NEW_RULER_ID :: ruler:id(),      % Ruler/Population id
    max_size   = 5     :: integer(),      % Population max agents
    runtime    = {0, 100}      :: runtime_specs(),  
    generation = {0, infinity} :: generation_specs(),   
    score      = {0, infinity} :: score_specs(),   
    champion   = undefined     :: agent:id(), % Best score agent_id                
    selection  = top3 :: selection:func()  % Selection for agents pool
}).
-type ruler() :: #ruler{}.

-record(agent, {
    id = ?NEW_AGENT_ID :: agent:id(),
    function    :: function(), % Function to perform
    arguments   :: term(),     % Arguments of the function
    mutation    :: function(), % Mutation to modify the arguments
    father      :: agent:id()  % Id of the agent one generation back
}). 
-type agent() :: #agent{}.


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns the record fields of an element.
%% @end
%%--------------------------------------------------------------------
-spec fields(Atom :: ruler | agent) -> ListOfFields :: [atom()].
fields(ruler) -> record_info(fields, ruler);
fields(agent) -> record_info(fields, agent).

%%--------------------------------------------------------------------
%% @doc
%% Creates a new agent on nndb and returns its Id
%% @end
%%--------------------------------------------------------------------
-spec agent(Properties :: agent:properties()) -> agent().
agent(Properties) ->
    Agent = #agent{id = ?NEW_AGENT_ID},
    edit(Agent, Properties).

%%--------------------------------------------------------------------
%% @doc Evaluates if the input is an agent.
%% @end
%%--------------------------------------------------------------------
-spec is_agent(Term :: term()) -> boolean().
is_agent(Agent) -> is_record(Agent, agent).

%%--------------------------------------------------------------------
%% @doc
%% Creates a new ruler on nndb and returns its Id
%% @end
%%--------------------------------------------------------------------
-spec ruler(Properties :: ruler:properties()) -> ruler().
ruler(Properties) ->
    Ruler = #ruler{id = ?NEW_RULER_ID},
    edit(Ruler, Properties).

%%--------------------------------------------------------------------
%% @doc Evaluates if the input is a ruler.
%% @end
%%--------------------------------------------------------------------
-spec is_ruler(Term :: term()) -> boolean().
is_ruler(Ruler) -> is_record(Ruler, ruler).

%%--------------------------------------------------------------------
%% @doc Edits an element using some properties.
%% @end
%%--------------------------------------------------------------------
-spec edit(Element, Properties) -> EditedElement when
    Element       :: agent() | ruler(),
    Properties    :: agent:properties() | ruler:properties(),
    EditedElement :: agent() | ruler().
edit(Agent, Properties) when is_record(Agent, agent) ->
    edit_agent(Agent, maps:to_list(Properties));
edit(Ruler, Properties) when is_record(Ruler, ruler) ->
    edit_ruler(Ruler, maps:to_list(Properties)).

edit_agent(Agent, [{       id, Value} | Options]) -> 
    edit_agent(Agent#agent{       id = Value}, Options);
edit_agent(Agent, [{ function, Value} | Options]) -> 
    edit_agent(Agent#agent{ function = Value}, Options);
edit_agent(Agent, [{arguments, Value} | Options]) -> 
    edit_agent(Agent#agent{arguments = Value}, Options);
edit_agent(Agent, [{ mutation, Value} | Options]) -> 
    edit_agent(Agent#agent{ mutation = Value}, Options);
edit_agent(Agent, [{   father, Value} | Options]) -> 
    edit_agent(Agent#agent{   father = Value}, Options);
edit_agent(Agent, []) -> 
    Agent.

edit_ruler(Ruler, [{        id, Value} | Options]) -> 
    edit_ruler(Ruler#ruler{        id = Value     }, Options); 
edit_ruler(Ruler, [{  max_size, Value} | Options]) -> 
    edit_ruler(Ruler#ruler{  max_size = Value     }, Options); 
edit_ruler(Ruler, [{  stoptime, Value} | Options]) -> 
    RunTime    = {Value, element(2, Ruler#ruler.runtime)},
    edit_ruler(Ruler#ruler{   runtime = RunTime   }, Options); 
edit_ruler(Ruler, [{   runtime, Value} | Options]) -> 
    edit_ruler(Ruler#ruler{   runtime = Value     }, Options); 
edit_ruler(Ruler, [{    oldest, Value} | Options]) -> 
    Generation = {Value, element(2, Ruler#ruler.generation)},
    edit_ruler(Ruler#ruler{generation = Generation}, Options); 
edit_ruler(Ruler, [{generation, Value} | Options]) -> 
    edit_ruler(Ruler#ruler{generation = Value     }, Options); 
edit_ruler(Ruler, [{    target, Value} | Options]) -> 
    Score      = {Value, element(2, Ruler#ruler.score)},
    edit_ruler(Ruler#ruler{    score = Score      }, Options); 
edit_ruler(Ruler, [{     score, Value} | Options]) -> 
    edit_ruler(Ruler#ruler{    score = Value      }, Options); 
edit_ruler(Ruler, [{ selection, Value} | Options]) -> 
    edit_ruler(Ruler#ruler{ selection = Value     }, Options); 
edit_ruler(Ruler, []) ->
    Ruler.

%%--------------------------------------------------------------------
%% @doc Returns the element id.
%% @end
%%--------------------------------------------------------------------
-spec id(Element :: agent() | ruler()) -> id().
id(Element) when is_record(Element, agent) -> Element#agent.id;
id(Element) when is_record(Element, ruler) -> Element#ruler.id.

%%--------------------------------------------------------------------
%% @doc Returns the agent function.
%% @end
%%--------------------------------------------------------------------
-spec function(Agent :: agent()) -> function().
function(Agent) when is_record(Agent, agent) -> Agent#agent.function.

%%--------------------------------------------------------------------
%% @doc Returns the agent arguments.
%% @end
%%--------------------------------------------------------------------
-spec arguments(Agent :: agent()) -> term().
arguments(Agent) when is_record(Agent,agent) -> Agent#agent.arguments.

%%--------------------------------------------------------------------
%% @doc Returns the agent mutation function.
%% @end
%%--------------------------------------------------------------------
-spec mutation(Agent :: agent()) -> term().
mutation(Agent) when is_record(Agent,agent) -> Agent#agent.mutation.

%%--------------------------------------------------------------------
%% @doc Returns the agent father.
%% @end
%%--------------------------------------------------------------------
-spec father(Agent :: agent()) -> agent:id() | undefined.
father(Agent) when is_record(Agent, agent) -> Agent#agent.father.

%%--------------------------------------------------------------------
%% @doc Clones an agent.
%% @end
%--------------------------------------------------------------------
-spec clone_agent(Agent :: agent()) -> 
    Clone :: agent().
clone_agent(Agent) -> 
    Agent#agent{id = ?NEW_AGENT_ID}.

%%--------------------------------------------------------------------
%% @doc Mutates an agent.
%% @end
%--------------------------------------------------------------------
-spec mutate_agent(Agent :: agent()) -> 
    Mutated :: agent().
mutate_agent(Agent) -> 
    Mutation_Function = Agent#agent.mutation,
    Agent#agent{
        id = ?NEW_AGENT_ID,
        arguments = Mutation_Function(Agent#agent.arguments),
        father    = Agent#agent.id
    }.

%%--------------------------------------------------------------------
%% @doc Clones a ruler.
%% @end
%--------------------------------------------------------------------
-spec clone_ruler(Ruler :: ruler()) -> 
    Clone :: ruler().
clone_ruler(Ruler) -> 
    Ruler#ruler{id = ?NEW_RULER_ID}.

%%--------------------------------------------------------------------
%% @doc Returns the genealogical tree of an agent
%% @end
%%--------------------------------------------------------------------
-spec tree(Agent :: agent() | agent:id()) -> 
    Tree :: [Agent :: agent:id()].
tree(Agent) when is_record(Agent, agent) -> 
    [id(Agent) | tree(Agent#agent.father)];
tree({_, agent} = Agent_Id) ->
    tree(ndb:read(Agent_Id));
tree(_Other) -> [].

%%--------------------------------------------------------------------
%% @doc Returns the population maximum size allowed by the ruler.
%% @end
%%--------------------------------------------------------------------
-spec max_size(Ruler :: ruler()) -> Max_Size :: integer().
max_size(Ruler) when is_record(Ruler, ruler) -> Ruler#ruler.max_size.

%%--------------------------------------------------------------------
%% @doc Returns the maximum running time allowed in milliseconds.
%% @end
%%--------------------------------------------------------------------
-spec runtime(Ruler :: ruler()) -> runtime_specs().
runtime(Ruler) when is_record(Ruler, ruler) -> Ruler#ruler.runtime.

%%--------------------------------------------------------------------
%% @doc Returns the ruler planed maximum generations.
%% @end
%%--------------------------------------------------------------------
-spec generation(Ruler :: ruler()) -> generation_specs().
generation(Ruler) when is_record(Ruler, ruler) -> 
    Ruler#ruler.generation.

%%--------------------------------------------------------------------
%% @doc Returns the ruler score target.
%% @end
%%--------------------------------------------------------------------
-spec score(Ruler :: ruler()) -> score_specs().
score(Ruler) when is_record(Ruler, ruler) -> Ruler#ruler.score.

%%--------------------------------------------------------------------
%% @doc Returns the ruler selection.
%% @end
%%--------------------------------------------------------------------
-spec selection(Ruler :: ruler()) -> selection:func().
selection(Ruler) when is_record(Ruler, ruler) -> Ruler#ruler.selection.


%%--------------------------------------------------------------------
%% @doc
%% TODO: Make description
%%
%% @end
%%--------------------------------------------------------------------
pformat(Element) when is_record(Element, agent) -> 
    pformat(Element, record_info(fields, agent));
pformat(Element) when is_record(Element, ruler) -> 
    pformat(Element, record_info(fields, ruler)).

pformat(Element, Fields) ->
    [io_lib:format("Element record: ~w ~n", [element(1, Element)]) |
     pformat(Element, Fields, 2)].

pformat(Element, [Field | ListOf_Fields], Index) ->
    [io_lib:format(" --> ~w = ~w ~n", [Field, element(Index, Element)]) |
     pformat(Element, ListOf_Fields, Index + 1)];
pformat(_Element, [], _Index) ->
    [].

%%====================================================================
%% Internal functions
%%====================================================================


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

