%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(eevo).
-author("borja").
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

%%% TODO -------------------------------------------------------------
% TODO: Change time by energy, agent dies if energy =< 0.0
% TODO: Old scores must be deleted as the context changes, performance?
% TODO: GUI based on web

-include_lib("society.hrl").

%% API
-export([]).

-define(EEVO_TABLES_ATTRIBUTES_LIST,
	[
		{population, record_info(fields, population)},
		{agent, record_info(fields, agent)}
	]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns the tables and fields used by the eevo application in
%% mnesia
%%
%% @end
%%--------------------------------------------------------------------
-spec(attributes_table() ->
	{Table_Name :: atom(), [Fields :: atom()]}).
attributes_table() ->
	?EEVO_TABLES_ATTRIBUTES_LIST.

%%--------------------------------------------------------------------
%% @doc
%% Creates a new population in the eevo database
%% TODO: Indicate the available options
%%
%% @end
%%--------------------------------------------------------------------
-spec(create_population() ->
	Population_Id :: population_id()).
create_population() ->
	_Population_Id = create_population([]).

-spec(create_population([{Option :: atom(), Value :: _}]) ->
	Population_Id :: population_id()).
create_population(Options) ->
	_Population_Id = {_, population} = demography:new_population(Options).

%%--------------------------------------------------------------------
%% @doc
%% Starts a population and its governor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_population(Population_Id :: population_id()) ->
	{ok, Governor_PId :: pid()}).
start_population(Population_Id) ->
	{ok, {Governor, _Pop_Sup}} = eevo_srv:run(Population_Id),
	put(Population_Id, Governor),
	{ok, Governor}.

%%--------------------------------------------------------------------
%% @doc
%% Stops a population, the governor and all agents
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop_population(Population_Id :: population_id()) ->
	ok | {error, not_found | simple_one_for_one}).
stop_population(Population_Id) ->
	eevo_srv:stop(Population_Id).

%%--------------------------------------------------------------------
%% @doc
%% Returns the governor PId of a population id
%%
%% @end
%%--------------------------------------------------------------------
-spec(governor(Population_Id :: population_id()) ->
	Governor :: pid()).
governor(Population_Id) ->
	case get(Population_Id) of
		Governor when is_pid(Governor) -> true = is_process_alive(Governor), Governor;
		undefined -> eevo_srv:governor(Population_Id)
	end.

%%--------------------------------------------------------------------
%% @doc
%% Creates a new population in the eevo database.
%% TODO: Indicate the available options
%%
%% @end
%%--------------------------------------------------------------------
-spec(create_agent(Module :: module(), AgentProperties :: term(),
                   MutationF :: function()) ->
	                  Agent_Id :: agent_id()).
create_agent(Module, AgentProperties, MutationF) ->
	_Agent_Id = create_agent(Module, AgentProperties, MutationF, []).

-spec(create_agent(Module :: module(), AgentProperties :: term(),
                   MutationF :: function(), [{Option :: atom(), Value :: _}]) ->
	                  Agent_Id :: agent_id()).
create_agent(Module, AgentProperties, MutationF, Options) ->
	_Agent_Id = {_, agent} = demography:new_agent(Module, AgentProperties, MutationF, Options).

%%--------------------------------------------------------------------
%% @doc
%% Requests a population governor to add an agent into its population
%%
%% @end
%%--------------------------------------------------------------------
-spec(add_agent(Population_Id :: population_id(), Agent_Id :: agent_id()) ->
	ok).
add_agent(Population_Id, Agent_Id) ->
	Governor = governor(Population_Id),
	ok = governor:async_queue(Governor, Agent_Id).

%%--------------------------------------------------------------------
%% @doc
%% Creates an agent into an specific population. Returns its PId
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_agent(Population_Id :: population_id(), Agent_Id :: agent_id()) ->
	{ok, Agent_PId :: pid()}).
start_agent(Population_Id, Agent_Id) ->
	Governor = governor(Population_Id),
	{ok, _Agent_PId} = governor:sync_queue(Governor, Agent_Id).

%%--------------------------------------------------------------------
%% @doc
%% Add a score value to an agent id. The function is asynchronous.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop_agent(Population_Id :: population_id(), Agent_Id :: agent_id()) ->
	ok).
stop_agent(Population_Id, Agent_Id) ->
	Governor = governor(Population_Id),
	ok = governor:stop(Governor, Agent_Id).

%%--------------------------------------------------------------------
%% @doc
%% Add a score value to an agent id. The function is asynchronous.
%%
%% @end
%%--------------------------------------------------------------------
-spec(add_score(Population_Id :: population_id(), Agent_Id :: agent_id(), Score :: float()) ->
	ok).
add_score(Population_Id, Agent_Id, Score) ->
	ok = add_score(Population_Id, Agent_Id, Score, _Score_Info = null).

-spec(add_score(Population_Id :: population_id(), Agent_Id :: agent_id(), Score :: float(),
                Score_Info :: term()) ->
	               ok).
add_score(Population_Id, Agent_Id, Score, Score_Info) ->
	Governor = governor(Population_Id),
	ok = governor:add_score(Governor, Agent_Id, Score, Score_Info).

%%--------------------------------------------------------------------
%% @doc
%% Sets a score value to an agent id. The function is asynchronous.
%%
%% @end
%%--------------------------------------------------------------------
-spec(set_score(Population_Id :: population_id(), Agent_Id :: agent_id(), Score :: float()) ->
	ok).
set_score(Population_Id, Agent_Id, Score) ->
	ok = set_score(Population_Id, Agent_Id, Score, _Score_Info = null).

-spec(set_score(Population_Id :: population_id(), Agent_Id :: agent_id(), Score :: float(),
                Score_Info :: term()) ->
	               ok).
set_score(Population_Id, Agent_Id, Score, Score_Info) ->
	Governor = governor(Population_Id),
	ok = governor:set_score(Governor, Agent_Id, Score, Score_Info).

%%--------------------------------------------------------------------
%% @doc
%% Returns the ETS with the score table managed by a governor
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_score_pool(Population_Id :: population_id()) ->
	Pool :: ets:tid()).
get_score_pool(Population_Id) ->
	Governor = governor(Population_Id),
	_Pool = governor:get_score_pool(Governor).

%%--------------------------------------------------------------------
%% @doc
%% Returns the N agents with the highest score
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_top(Population_Id :: population_id(), N :: integer()) ->
	[{Agent_Id :: agent_id(), Score :: float(), Additional_Info :: term()}]).
get_top(Population_Id, N) ->
	Pool = get_score_pool(Population_Id),
	get_last_n_from_pool(Pool, ets:last(Pool), N).

get_last_n_from_pool(_Pool, '$end_of_table', _N) -> [];
get_last_n_from_pool(_Pool, _ScoreAgent, 0)      -> [];
get_last_n_from_pool(Pool, ScoreAgent, N) ->
	[{{Score, Agent_Id}, Additional_Info}] = ets:lookup(Pool, ScoreAgent),
	[{Agent_Id, Score, Additional_Info} | get_last_n_from_pool(Pool, ets:prev(Pool, ScoreAgent), N - 1)].

%%--------------------------------------------------------------------
%% @doc
%% Returns the N agents with the lowest score
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_bottom(Population_Id :: population_id(), N :: integer()) ->
	[{Agent_Id :: agent_id(), Score :: float(), Additional_Info :: term()}]).
get_bottom(Population_Id, N) ->
	Pool = get_score_pool(Population_Id),
	get_first_n_from_pool(Pool, ets:first(Pool), N).

get_first_n_from_pool(_Pool, '$end_of_table', _N) -> [];
get_first_n_from_pool(_Pool, _ScoreAgent, 0)      -> [];
get_first_n_from_pool(Pool, ScoreAgent, N) ->
	[{{Score, Agent_Id}, Additional_Info}] = ets:lookup(Pool, ScoreAgent),
	[{Agent_Id, Score, Additional_Info} | get_first_n_from_pool(Pool, ets:next(Pool, ScoreAgent), N - 1)].

%%--------------------------------------------------------------------
%% @doc
%% Returns the genealogical tree of an agent
%%
%% @end
%%--------------------------------------------------------------------
-spec(genealogical_tree(Population_Id :: population_id()) ->
	Genealogical_Tree :: [agent_id()]).
genealogical_tree(Agent_Id) ->
	_Genlg_Tree = demography:genealogical_tree(Agent_Id).

%%--------------------------------------------------------------------
%% @doc
%% Reads the father agent, applies the mutation function to the agent
%% properties, saves the mutated agent on the nndb and returns its Id
%%
%% @end
%%--------------------------------------------------------------------
-spec(mutate_agent(Agent_Id :: agent_id()) ->
	MutatedAgent_Id:: agent_id()).
mutate_agent(Agent_Id) ->
	Agent = #agent{properties = AgentProperties, mutation_f = MutationF} = nndb:read(Agent_Id),
	MutatedAgent = Agent#agent{
		id         = ?AGENT_ID(nnref:new()),
		properties = MutationF(AgentProperties),
		father     = Agent_Id},
	nndb:write(MutatedAgent),
	MutatedAgent#agent.id.

%%--------------------------------------------------------------------
%% @doc
%% Pretty formats an eevo record
%%
%% @end
%%--------------------------------------------------------------------
-spec(pformat(Population_Id :: population_id()) ->
	Pretty_Format :: string()).
pformat(Element) ->
	_Pretty_Format = demography:pformat(Element).

%%%===================================================================
%%% Internal functions
%%%===================================================================





