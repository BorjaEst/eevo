%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(eevo_srv).
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("society.hrl").

-behaviour(gen_server).

%% API
%%-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(MAX_PARALLEL_POPULATIONS, 4).
-record(state, {
	limit = ?MAX_PARALLEL_POPULATIONS :: integer(),
	populations = orddict:new() :: [{Pop_Id :: population_id(), {Gov :: pid(), Pop_Sup :: pid()}}],
	refs = gb_trees:empty() :: {Ref :: reference(), Pop_Id :: population_id()},
	queue = queue:new(),
	report_path = ?DEFAULT_RESULTS_PATH
}).

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
start_link(StartArgs) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, StartArgs, []).

%%--------------------------------------------------------------------
%% @doc
%% % TODO: To make description
%%
%% @end
%%--------------------------------------------------------------------
run(Population_Id) ->
	{ok, {_Gov, _Pop_Sup}} = gen_server:call(?SERVER, {run, Population_Id}, ?STDCALL_TIMEOUT).

%%--------------------------------------------------------------------
%% @doc
%% % TODO: To make description
%%
%% @end
%%--------------------------------------------------------------------
stop(Population_Id) ->
	_Result = gen_server:call(?SERVER, {stop, Population_Id}, ?STDCALL_TIMEOUT).

%%--------------------------------------------------------------------
%% @doc
%% % TODO: To make description
%%
%% @end
%%--------------------------------------------------------------------
governor(Population_Id) ->
	case gen_server:call(?SERVER, {population, Population_Id}, ?STDCALL_TIMEOUT) of
		{ok, {Gov, _Pop_Sup}} -> Gov;
		Error -> Error
	end.

%%--------------------------------------------------------------------
%% @doc
%% % TODO: To make description
%%
%% @end
%%--------------------------------------------------------------------
pop_sup(Population_Id) ->
	case gen_server:call(?SERVER, {population, Population_Id}, ?STDCALL_TIMEOUT) of
		{ok, {_Gov, Pop_Sup}} -> Pop_Sup;
		Error -> Error
	end.

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
init(StartArgs) ->
	do_init(StartArgs, #state{}).

do_init([{report_path, Path} | StartArgs], State) ->
	do_init(StartArgs, State#state{report_path = Path});
do_init([], State) ->
	file:make_dir(State#state.report_path),
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

handle_call({run, Pop_Id}, From, #state{limit = L, populations = Populations} = State) when L > length(Populations) ->
	handle_start_population(Pop_Id, State, From);
handle_call({run, Pop_Id}, From, State) ->
	#state{queue = Q} = State,
	{noreply, State#state{queue = queue:in({From, Pop_Id}, Q)}};

handle_call({stop, Pop_Id}, _From, State) ->
	Reply = eevo_sup:terminate_population_supervisor(Pop_Id),
	{reply, Reply, State};

handle_call({population, Pop_Id}, _From, State) ->
	Reply = orddict:find(Pop_Id, State#state.populations),
	{reply, Reply, State};

handle_call(Request, From, State) ->
	?LOG_WARNING("Unknown handle_call eevo_srv, request ~p, from ~p", [Request, From]),
	{reply, ok, State}.

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
handle_cast(Request, State) ->
	?LOG_WARNING("Unknown handle_cast eevo_srv, request ~p", [Request]),
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

handle_info({'DOWN', Ref, process, PId, Info}, State) ->
%%	?LOG({"RIP Population: ", PId, Info}),
	case gb_trees:is_defined(Ref, State#state.refs) of
		true -> handle_down_population(Ref, State);
		false -> error({"Incorrect 'DOWN' message", {'DOWN', Ref, process, PId, Info}})
	end;

handle_info(Info, State) ->
	?LOG_WARNING("Unknown handle_info eevo_srv, info ~p", [Info]),
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
terminate(Reason, _State) ->
	?LOG_INFO("terminate eevo_srv, reason ~p", [Reason]),
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
handle_start_population(Population_Id, State, {Owner, _Tag} = _From) ->
	#state{refs = Refs, populations = Populations} = State,
	{ok, Pop_Sup} = eevo_sup:start_population_supervisor(Population_Id),
	{ok, Gov} = pop_sup:start_governor(Pop_Sup, #{
		id           => Population_Id,
		owner        => Owner,
		report_path => State#state.report_path
	}),
	Ref = erlang:monitor(process, Pop_Sup),
	{reply, {ok, {Gov, Pop_Sup}}, State#state{populations = orddict:store(Population_Id, {Gov, Pop_Sup}, Populations),
	                                          refs        = gb_trees:insert(Ref, Population_Id, Refs)}}.

% ......................................................................................................................
handle_down_population(Ref, State) ->
	#state{refs = Refs, populations = Populations, queue = Queue} = State,
	DeadPopulation_Id = gb_trees:get(Ref, Refs),
	UpdtState = State#state{
		populations = orddict:erase(DeadPopulation_Id, Populations),
		refs        = gb_trees:delete(Ref, Refs)},
	case queue:out(Queue) of
		{{value, {From, NewPopulation_Id}}, Q} ->
			{_, Reply, NewState} = handle_start_population(NewPopulation_Id, UpdtState, From),
			gen_server:reply(From, Reply),
			{noreply, NewState#state{queue = Q}};
		{empty, _} ->
			{noreply, UpdtState}
	end.
