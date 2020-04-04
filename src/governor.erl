%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(governor).
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("society.hrl").

-behaviour(gen_server).

%% API
%%-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(ETS_TABLE_OPTIONS, [ordered_set]).
-define(INIT_AGENT_SCORE, 0.0).
-record(state, {
	id :: population_id(),
	limit :: integer(),
	minimum :: integer(),
	start_time = erlang:monotonic_time(millisecond) :: integer(),
	agents_counter = 0 :: integer(),
	spawn_limit :: integer() | infinity,
	score_limit :: float(),
	evo_alg_f :: function(), % Specifies the evolutionary loop function,
	sel_alg_f :: function(),
	owner :: pid(),
	agents_sup :: pid(),
	score_pool = ets:new(score_pool, ?ETS_TABLE_OPTIONS) :: reference(), % Ordered by SCORE
	agents = orddict:new() :: [{Agent_Id :: agent_id(), Score :: float()}],
	refs = gb_trees:empty() :: {Ref :: reference(), Agent_Id :: agent_id()},
	queue = queue:new(),
	report_path :: string()}).

-ifdef(debug_mode).
-define(STDCALL_TIMEOUT, infinity).
-else.
-define(STDCALL_TIMEOUT, 5000).
-endif.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% % TODO: To make description
%%
%% @end
%%--------------------------------------------------------------------
start_link(Arguments) ->
	gen_server:start_link(?MODULE, [{supervisor, self()} | Arguments], []).

%%--------------------------------------------------------------------
%% @doc
%% % TODO: To make description
%%
%% @end
%%--------------------------------------------------------------------
sync_queue(Governor, Agent_Id) ->
	gen_server:call(Governor, {sync, Agent_Id}, ?STDCALL_TIMEOUT).

%%--------------------------------------------------------------------
%% @doc
%% % TODO: To make description
%%
%% @end
%%--------------------------------------------------------------------
async_queue(Governor, Agent_Id) ->
	gen_server:cast(Governor, {async, Agent_Id}).

%%--------------------------------------------------------------------
%% @doc
%% % TODO: To make description
%%
%% @end
%%--------------------------------------------------------------------
stop(Governor, Agent_Id) ->
	gen_server:call(Governor, {stop, Agent_Id}, ?STDCALL_TIMEOUT).


%%--------------------------------------------------------------------
%% @doc
%% % TODO: To make description
%%
%% @end
%%--------------------------------------------------------------------
add_score(Governor, Agent_Id, Score, Score_Info) ->
	gen_server:cast(Governor, {add_score, Agent_Id, Score, Score_Info}).

%%--------------------------------------------------------------------
%% @doc
%% % TODO: To make description
%%
%% @end
%%--------------------------------------------------------------------
set_score(Governor, Agent_Id, Score, Score_Info) ->
	gen_server:cast(Governor, {set_score, Agent_Id, Score, Score_Info}).

%%--------------------------------------------------------------------
%% @doc
%% % TODO: To make description
%%
%% @end
%%--------------------------------------------------------------------
get_score_pool(Governor) ->
	gen_server:call(Governor, get_score_pool, ?STDCALL_TIMEOUT).

%%--------------------------------------------------------------------
%% @doc
%% % TODO: To make description
%%
%% @end
%%--------------------------------------------------------------------
trigger_evolution(Governor) ->
	gen_server:cast(Governor, trigger_evolution).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
	{ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init(Arguments) ->
	do_init(Arguments, #state{}).

do_init([{owner, Value} | Arguments], State) ->
	do_init(Arguments, State#state{owner = Value});
do_init([{report_path, Value} | Arguments], State) ->
	do_init(Arguments, State#state{report_path = Value});
do_init([{supervisor, PopSup} | Arguments], State) ->
	self() ! {start_agents_supervisor, PopSup},
	do_init(Arguments, State);
do_init([{id, Value} | Arguments], State) ->
	Population = nndb:read(Value),
	timer:send_after(Population#population.run_time, run_end),
	do_init(Arguments, State#state{
		id          = Population#population.id,
		limit       = Population#population.limit,
		minimum     = Population#population.minimum,
		spawn_limit = Population#population.run_agents,
		score_limit = Population#population.run_score,
		evo_alg_f   = Population#population.evo_alg_f,
		sel_alg_f   = Population#population.sel_alg_f
	});
do_init([], State) ->
	report:new(State#state.report_path, ?ID_FILEMODE(State#state.id)),
	process_flag(trap_exit, true),
	?LOG_INFO("Population_Id:~p initiated", [State#state.id]),
	{ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #state{}) ->
	                 {reply, Reply :: term(), NewState :: #state{}} |
	                 {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
	                 {noreply, NewState :: #state{}} |
	                 {noreply, NewState :: #state{}, timeout() | hibernate} |
	                 {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
	                 {stop, Reason :: term(), NewState :: #state{}}).

handle_call({sync, Agent_Id}, _From, #state{limit = L, agents = Agents} = State) when L > length(Agents) ->
	?LOG_INFO("handle_call Population_Id:~p --> agent ~p start", [State#state.id, Agent_Id]),
	{Ref, PId} = handle_start_agent(Agent_Id, State#state.id, State#state.agents_sup),
	{reply, {ok, PId}, State#state{
		agents = orddict:store(Agent_Id, ?INIT_AGENT_SCORE, Agents),
		refs   = gb_trees:insert(Ref, Agent_Id, State#state.refs)}
	};
handle_call({sync, Agent_Id}, From, State) ->
	?LOG_INFO("handle_call Population_Id:~p --> agent ~p queue", [State#state.id, Agent_Id]),
	?LOG_DEBUG("Agent ~p in queue: ~p", [Agent_Id, State#state.queue]),
	{noreply, State#state{
		queue = queue:in({reply, From, Agent_Id}, State#state.queue)}
	};

handle_call({stop, Agent_Id}, _From, State) ->
	?LOG_INFO("handle_call Population_Id:~p --> agent ~p stop", [State#state.id, Agent_Id]),
	#state{agents_sup = Agents_Sup} = State,
	agents_sup:stop_agent(Agents_Sup, Agent_Id),
	{reply, ok, State};

handle_call(get_score_pool, _From, State) ->
	?LOG_INFO("handle_call Population_Id:~p --> get score pool", [State#state.id]),
	#state{score_pool = ScorePool} = State,
	{reply, ScorePool, State};

handle_call(Request, _From, State) ->
	?LOG_WARNING("Unknown handle_call Population_Id ~p, request ~p", [State#state.id, Request]),
	{reply, {error, {badrequest, Request}}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).

handle_cast({async, Agent_Id}, #state{limit = L, agents = Agents} = State) when L > length(Agents) ->
	?LOG_INFO("handle_cast Population_Id:~p --> agent ~p start", [State#state.id, Agent_Id]),
	{Ref, _PId} = handle_start_agent(Agent_Id, State#state.id, State#state.agents_sup),
	{noreply, State#state{
		agents = orddict:store(Agent_Id, ?INIT_AGENT_SCORE, Agents),
		refs   = gb_trees:insert(Ref, Agent_Id, State#state.refs)}
	};
handle_cast({async, Agent_Id}, State) ->
	?LOG_INFO("handle_cast Population_Id:~p --> agent ~p queue", [State#state.id, Agent_Id]),
	?LOG_DEBUG("Agent ~p in queue: ~p", [Agent_Id, State#state.queue]),
	{noreply, State#state{
		queue = queue:in({noreply, Agent_Id}, State#state.queue)}
	};

handle_cast(trigger_evolution, State) ->
	?LOG_INFO("handle_cast Population_Id:~p --> trigger evolution", [State#state.id]),
	{noreply, handle_evolution(State)};

handle_cast({add_score, Agent_Id, Score, Score_Info}, State) ->
	?LOG_INFO("handle_cast Population_Id:~p --> agent ~p add score ~p", [State#state.id, Agent_Id, Score]),
	?LOG_DEBUG("Agent_Id ~p Score_Info ~p", [Agent_Id, Score_Info]),
	case orddict:find(Agent_Id, State#state.agents) of
		{ok, OldScore} -> {noreply, handle_set_score(Agent_Id, Score + OldScore, OldScore, Score_Info, State)};
		error -> error({"Score message for an unknown agent", {score, Agent_Id, Score}})
	end;
handle_cast({set_score, Agent_Id, Score, Score_Info}, State) ->
	?LOG_INFO("handle_cast Population_Id:~p --> agent ~p set score ~p", [State#state.id, Agent_Id, Score]),
	?LOG_DEBUG("Agent_Id ~p Score_Info ~p", [Agent_Id, Score_Info]),
	case orddict:find(Agent_Id, State#state.agents) of
		{ok, OldScore} -> {noreply, handle_set_score(Agent_Id, Score, OldScore, Score_Info, State)};
		error -> error({"Score message for an unknown agent", {score, Agent_Id, Score}})
	end;

handle_cast(Request, State) ->
	?LOG_WARNING("Unknown handle_cast Population_Id ~p, request ~p", [State#state.id, Request]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).

handle_info({'DOWN', DownRef, process, _DownPId, _Info}, State) ->
	{DownAgent_Id, UpdatedRefs} = gb_trees:take(DownRef, State#state.refs),
	?LOG_INFO("handle_info Population_Id:~p --> agent ~p down", [State#state.id, DownAgent_Id]),
	Score = orddict:fetch(DownAgent_Id, State#state.agents),
	?LOG_DEBUG("DOWN Agent {Score, Agent_Id}:~p", [{Score, DownAgent_Id}]),
	catch report_agent(DownAgent_Id, Score, State#state.score_pool), % Save agent data into a file
	{Out, UpdatedQueue} = queue:out(State#state.queue),
	{noreply, handle_queue(Out, State#state{
		agents_counter = State#state.agents_counter + 1,
		agents         = orddict:erase(DownAgent_Id, State#state.agents),
		queue          = UpdatedQueue,
		refs           = UpdatedRefs
	})};

handle_info({start_agents_supervisor, PopSup}, State) ->
	?LOG_INFO("handle_info Population_Id:~p --> start agents supervisor", [State#state.id]),
	{ok, Agents_Sup} = pop_sup:start_agents_supervisor(PopSup, State#state.id),
	{noreply, State#state{agents_sup = Agents_Sup}};

handle_info(run_end, State) ->
	?LOG_INFO("handle_info Population_Id:~p --> run end", [State#state.id]),
	State#state.owner ! {run_end, State#state.id, population_result(State)},
	{stop, normal, State};

handle_info(Info, State) ->
	?LOG_WARNING("Unknown handle_info Population_Id ~p, info ~p", [State#state.id, Info]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #state{}) -> term()).
terminate(Reason, State) ->
	?LOG_INFO("terminate Population_Id:~p, reason ~p", [State#state.id, Reason]),
	report:close(),
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
                  Extra :: term()) ->
	                 {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

% ......................................................................................................................
population_result(State) ->
	_Result = #{
		best_score   => ets:last(State#state.score_pool),
		n_agents     => State#state.agents_counter,
		running_time => (erlang:monotonic_time(millisecond) - State#state.start_time)}.

% ......................................................................................................................
handle_start_agent(Agent_Id, Population_Id, Agents_Sup) ->
	{ok, PId} = agents_sup:start_agent(Agents_Sup, Population_Id, Agent_Id),
	{erlang:monitor(process, PId), PId}.

% ......................................................................................................................
handle_queue(Out, #state{agents_counter = Counter, spawn_limit = Limit} = State) when Counter >= Limit ->
	self() ! run_end,
	handle_queue(Out, State#state{
		spawn_limit = alert_raised
	});
handle_queue({value, {reply, From, Agent_Id}}, State) ->
	{Ref, PId} = handle_start_agent(Agent_Id, State#state.id, State#state.agents_sup),
	gen_server:reply(From, {ok, PId}),
	State#state{
		agents = orddict:store(Agent_Id, ?INIT_AGENT_SCORE, State#state.agents),
		refs   = gb_trees:insert(Ref, Agent_Id, State#state.refs)
	};
handle_queue({value, {noreply, Agent_Id}}, State) ->
	{Ref, _PId} = handle_start_agent(Agent_Id, State#state.id, State#state.agents_sup),
	State#state{
		agents = orddict:store(Agent_Id, ?INIT_AGENT_SCORE, State#state.agents),
		refs   = gb_trees:insert(Ref, Agent_Id, State#state.refs)
	};
handle_queue(empty, State) ->
	handle_evolution(State).

% ......................................................................................................................
handle_set_score(A_Id, NewScore, OldScore, S_Info, #state{score_limit = Limit} = State) when NewScore >= Limit ->
	self() ! run_end,
	handle_set_score(A_Id, NewScore, OldScore, S_Info, State#state{score_limit = alert_raised});
handle_set_score(Agent_Id, NewScore, OldScore, Score_Info, State) ->
	#state{agents = Agents, score_pool = ScorePool} = State,
	ets:delete(ScorePool, {OldScore, Agent_Id}),
	ets:insert(ScorePool, {{NewScore, Agent_Id}, Score_Info}),
	State#state{agents = orddict:store(Agent_Id, NewScore, Agents)}.

% ......................................................................................................................
scoreAgents(Pool) -> scoreAgents(Pool, ets:last(Pool)).

scoreAgents(Pool, {_Score, _Agent_Id} = Key) -> [Key | scoreAgents(Pool, ets:prev(Pool, Key))];
scoreAgents(_Pool, '$end_of_table')          -> [].

% ......................................................................................................................
handle_evolution(State) ->
	#state{limit      = Limit,
	       score_pool = ScorePool,
	       agents     = Agents,
	       evo_alg_f  = EvolutionF,
	       sel_alg_f  = SelectionF} = State,
	ScoreAgents_List = scoreAgents(ScorePool),
	NClonesToGenerate = EvolutionF(Limit, length(Agents)),
	AgentsToMutate = SelectionF(ScoreAgents_List, NClonesToGenerate),
	ListOfNewAgents = [eevo:mutate_agent(Agent_Id) || Agent_Id <- AgentsToMutate],
	_NewState = lists:foldl(fun(A_Id, S) -> handle_run_agent(A_Id, S) end, State, ListOfNewAgents).

handle_run_agent(Agent_Id, #state{limit = L, agents = Agents} = State) when L > length(Agents) ->
	{Ref, _PId} = handle_start_agent(Agent_Id, State#state.id, State#state.agents_sup),
	State#state{
		agents = orddict:store(Agent_Id, ?INIT_AGENT_SCORE, Agents),
		refs   = gb_trees:insert(Ref, Agent_Id, State#state.refs)};
handle_run_agent(Agent_Id, State) ->
	State#state{
		queue = queue:in(Agent_Id, State#state.queue)}.

% ......................................................................................................................
report_agent(Agent_Id, Score, Pool) ->
	[{{Score, Agent_Id}, Additional_Info}] = ets:lookup(Pool, {Score, Agent_Id}),
	Agent = nndb:read(Agent_Id),
	report:map(
		#{
			id          => report:format_tuple(Agent_Id),
			score       => Score,
			extra_info  => report:format_any(Additional_Info),
			module      => Agent#agent.module,
			properties  => report:format_any(Agent#agent.properties),
			mutation_f  => report:format_function(Agent#agent.mutation_f),
			behaviour   => Agent#agent.behaviour,
			father      => report:format_any(Agent#agent.father)
		}).


%%====================================================================
%% Eunit white box tests
%%====================================================================

% ----------------------------------------------------------------------------------------------------------------------
% TESTS DESCRIPTIONS ---------------------------------------------------------------------------------------------------
scoreAgents_test_() ->
	% {setup, Where, Setup, Cleanup, Tests | Instantiator}
	[
		{"Return the score list of agents from best to worse",
		 {setup, local, fun no_setup/0, fun no_cleanup/1, fun test_scoreAgents_normal/1}},
		{"When list is empty returns an empty list",
		 {setup, local, fun no_setup/0, fun no_cleanup/1, fun test_scoreAgents_empty/1}}
	
	].

% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS ---------------------------------------------------------------------------------------------
no_setup() ->
	ok.

no_cleanup(_) ->
	ok.

% ----------------------------------------------------------------------------------------------------------------------
% ACTUAL TESTS ---------------------------------------------------------------------------------------------------------

test_scoreAgents_normal(_) ->
	Pool = ets:new(simple_agents, ?ETS_TABLE_OPTIONS),
	ScoreAgents = [{{_Score = rand:uniform(), _Agent_Id = {agent, N}}, no_info} || N <- lists:seq(1, 10)],
	[ets:insert(Pool, {Key, no_info}) || Key <- ScoreAgents],
	[
		?_assertEqual(lists:reverse(lists:sort(ScoreAgents)), scoreAgents(Pool))
	].

test_scoreAgents_empty(_) ->
	Pool = ets:new(simple_agents, ?ETS_TABLE_OPTIONS),
	[
		?_assertEqual([], scoreAgents(Pool))
	].


% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS --------------------------------------------------------------------------------------------

