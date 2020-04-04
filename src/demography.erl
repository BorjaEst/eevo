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
    id = ?NEW_RULER_ID :: ruler:id(),
    size       = 5     :: integer(),      % Aagents per population
    run_time   = infinity :: integer(),   % Time(ms) before end
    run_agents = infinity :: integer(),   % Tested agents before end
    run_score  = infinity :: float(),     % Score target before end
    selection  = top3 :: selection:func() % Selection for agents pool
}).
-type ruler() :: #ruler{}.

-record(agent, {
    id = ?NEW_AGENT_ID :: agent:id(),
    function    :: function(), % Function to perform
    arguments   :: term(),     % Arguments of the function
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

edit_agent(Agent, [{id,              Value} | Options]) ->
   edit_agent(Agent#agent{id = Value}, Options);
%%% TODO: To define
edit_agent(Agent, []) -> 
    Agent.

edit_ruler(Ruler, [{id,          Value} | Options]) -> 
    edit_ruler(Ruler#ruler{id = Value}, Options);
%%% TODO: To define
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





