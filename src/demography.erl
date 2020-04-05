%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(demography).
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

-include_lib("eunit/include/eunit.hrl").

%% API
%%-export([]).
-export_type([type/0, ruler/0, agent/0]).
-export_type([]).

%%% Populations are composed mostly by 2 types:
-type type()   ::      ruler |      agent.
-type id()     :: ruler:id() | agent:id().

-define(NEW_RULER_ID,       {make_ref(), ruler}).
-define(NEW_AGENT_ID,       {make_ref(), agent}).
-define(REFERENCE(Id), element(2,element(1,Id))).

-record(ruler, {
    id = ?NEW_RULER_ID :: ruler:id(),      % Ruler/Population id
    max_size    = 5     :: integer(),      % Population max agents
    stop_time   = infinity :: integer(),   % Time(ms) before end
    generations = infinity :: integer(),   % Tested agents before end
    target      = infinity :: float(),     % Score target before end
    selection = top3 :: selection:func()  % Selection for agents pool
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
% TODO: specs
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
% TODO: specs
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

edit_agent(Agent, [{id,        Value} | Options]) -> 
    edit_agent(Agent#agent{       id = Value}, Options);
edit_agent(Agent, [{function,  Value} | Options]) -> 
    edit_agent(Agent#agent{ function = Value}, Options);
edit_agent(Agent, [{arguments, Value} | Options]) -> 
    edit_agent(Agent#agent{arguments = Value}, Options);
edit_agent(Agent, [{mutation,  Value} | Options]) -> 
    edit_agent(Agent#agent{ mutation = Value}, Options);
edit_agent(Agent, [{father,    Value} | Options]) -> 
    edit_agent(Agent#agent{   father = Value}, Options);
edit_agent(Agent, []) -> 
    Agent.

edit_ruler(Ruler, [{id,          Value} | Options]) -> 
    edit_ruler(Ruler#ruler{         id = Value}, Options); 
edit_ruler(Ruler, [{max_size,    Value} | Options]) -> 
    edit_ruler(Ruler#ruler{   max_size = Value}, Options); 
edit_ruler(Ruler, [{stop_time,   Value} | Options]) -> 
    edit_ruler(Ruler#ruler{  stop_time = Value}, Options); 
edit_ruler(Ruler, [{generations, Value} | Options]) -> 
    edit_ruler(Ruler#ruler{generations = Value}, Options); 
edit_ruler(Ruler, [{target,      Value} | Options]) -> 
    edit_ruler(Ruler#ruler{     target = Value}, Options); 
edit_ruler(Ruler, [{selection,   Value} | Options]) -> 
    edit_ruler(Ruler#ruler{  selection = Value}, Options); 
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
-spec max_sixe(Ruler :: ruler()) -> Max_Size :: integer().
max_sixe(Ruler) when is_record(Ruler, ruler) -> Ruler#ruler.max_size.

%%--------------------------------------------------------------------
%% @doc Returns the maximum running time allowed in milliseconds.
%% @end
%%--------------------------------------------------------------------
-spec stop_time(Ruler :: ruler()) -> StopTime :: integer().
stop_time(Ruler) when is_record(Ruler, ruler) -> Ruler#ruler.stop_time.

%%--------------------------------------------------------------------
%% @doc Returns the ruler planed maximum generations.
%% @end
%%--------------------------------------------------------------------
-spec generations(Ruler :: ruler()) -> generations.
generations(Ruler) when is_record(Ruler, ruler) -> 
    Ruler#ruler.generations.

%%--------------------------------------------------------------------
%% @doc Returns the ruler score target.
%% @end
%%--------------------------------------------------------------------
-spec target(Ruler :: ruler()) -> ScoreTarget :: float().
target(Ruler) when is_record(Ruler, ruler) -> Ruler#ruler.target.

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





