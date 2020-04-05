%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ruler).
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("society.hrl").

-behaviour(gen_server).

%% API
%%-export([start_link/0]).
-export_types([id/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-type id() :: {Ref :: reference(), ruler}.
-type property()   :: todefine().
-type properties() :: #{
    OptionalProperty :: property() => Value :: term()
}.

-define(POOL_UPDATE_INTERVAL, 100).
-define(CLEAN_DEAD_INTERVAL,   90).
-define(ETS_TABLE_OPTIONS, [ordered_set]).
-define(INIT_SCORE, 0.0).
-record(state, {
    size       :: {Current :: integer(), Max :: integer()},
    run_time   :: {Current :: integer(), End :: integer()},
    generation :: {Current :: integer(), End :: integer()}, 
    best_score :: {Current ::   float(), End ::   float()},   
    selection  :: selection:func(),
    agents     :: #{Agent_Id  :: agent:id()  => Info :: info()},
    refs       :: #{Reference :: reference() => agent:id()},
    queue      :: queue:queua()
 }).
 -type info() :: {Score :: float(), Pid :: pid()}.

-define(LOG_HANDLE_CAST_MESSAGE(Message, State),
    ?LOG_DEBUG(#{what => "Received cast request", pid => self(),
                 details => #{message => Message,
                              agents  => State#state.agents,
                              queue   => State#state.queue}},
               #{logger_formatter=>#{title=>"RULER REQUEST"}})
).
-define(LOG_UNKNOWN_AGENT(Agent_Id, Agents),
    ?LOG_DEBUG(#{what => "Request for unknow agent", pid => self(),
                 details => #{agent_id => Agent_Id,
                              agents   => Agents}},
               #{logger_formatter=>#{title=>"RULER REQUEST"}})
).


-ifdef(debug_mode).
-define(STDCALL_TIMEOUT, infinity).
-else.
-define(STDCALL_TIMEOUT, 5000).
-endif.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the population ruler.
%% @end
%%--------------------------------------------------------------------
-spec start_link(Ruler_Id :: id()) -> gen_server:start_ret().
start_link(Ruler_Id) ->
    gen_server:start_link(?MODULE, [Ruler_Id, self()], []).

%%--------------------------------------------------------------------
%% @doc Request the agent addition to the population. Asynchronous.
%% @end
%%--------------------------------------------------------------------
-spec async_queue(Ruler :: pid(), Agent_Id :: agent:id()) ->
    ok.
async_queue(Ruler, Agent_Id) ->
    gen_server:cast(Ruler, {queue, Agent_Id}).

%%--------------------------------------------------------------------
%% @doc Request the kill of an agent. Asynchronous.
%% @end
%%--------------------------------------------------------------------
-spec async_kill(Ruler :: pid(), Agent_Id :: agent:id()) ->
    ok.
async_kill(Ruler, Agent_Id) ->
    gen_server:cast(Ruler, {kill, Agent_Id}).

%%--------------------------------------------------------------------
%% @doc Adds a score to an agent. Asynchronous.
%% @end
%%--------------------------------------------------------------------
-spec score(Ruler, Agent_Id, Score) -> ok when 
    Ruler    :: pid(), 
    Agent_Id :: agent:id(), 
    Score    :: float().
score(Ruler, Agent_Id, Score) ->
    gen_server:cast(Ruler, {score, Agent_Id, Score}).

%%--------------------------------------------------------------------
%% @doc Returns the score pool ets table id.
%% @end
%%--------------------------------------------------------------------
-spec score_pool(Ruler_Id :: id()) -> ets:tid().
score_pool(Ruler_Id) ->
    ets:lookup_element(?EV_POOL, Ruler_Id, #population.score_pool).


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
-spec init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
init([Id, Supervisor]) ->
    Ruler      = edb:read(Id),    
    Score_Pool = ets:new(undef, ?ETS_TABLE_OPTIONS),
    put(   id,          Id), % Used when updating the eevo_pool
    put(   sup, Supervisor), % Used when spawn agents under the OTP
    put(agents,         []), % Used when cleaning dead from agents#{}
    put( spool, Score_Pool), % Used when scoring agents
    true = ets:update_element(?EV_POOL, Id, [
        {     #population.ruler,     self()},
        {#population.score_pool, Score_Pool}
    ]),
    process_flag(trap_exit, true),
    ?LOG_INFO("Population_Id:~p initiated", [Id]),
    self() ! update_pool, % Request to update pool after start
    timer:send_interval(?POOL_UPDATE_INTERVAL, update_pool),
    timer:send_interval(?CLEAN_DEAD_INTERVAL,   clean_dead),
    timer:send_after(demography:stop_time(Ruler), run_end),
    {ok, #state{
        size       = {  0,    demography:max_size(Ruler)},
        run_time   = {  0,   demography:stop_time(Ruler)},
        generation = {  0, demography:generations(Ruler)}, 
        best_score = {0.0,      demography:target(Ruler)},   
        selection  = demography:selection(Ruler),
        agents     = #{},
        refs       = #{},
        queue      = queue:new()
    }}.


%%--------------------------------------------------------------------
%% @private
%% @doc Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #state{}) ->
                     {reply, Reply :: term(), NewState :: #state{}} |
                     {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
                     {noreply, NewState :: #state{}} |
                     {noreply, NewState :: #state{}, timeout() | hibernate} |
                     {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
                     {stop, Reason :: term(), NewState :: #state{}}.
handle_call(Request, _From, State) ->
    ?LOG_WARNING("Unknown handle_call Population_Id ~p, request ~p", 
                 [get(id), Request]),
    {reply, {error, badrequest}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}.
handle_cast({queue, Agent_Id} = Message, State) ->
    ?LOG_HANDLE_CAST_MESSAGE(Message, State),
    {noreply, State#state{
        queue = queue:in(Agent_Id, State#state.queue)
    }};
handle_cast({kill, Agent_Id} = Message, State) ->
    ?LOG_HANDLE_CAST_MESSAGE(Message, State),
    case maps:get(Agent_Id, State#state.agents, error) of
        {_,dead} -> already_dead;
        {_, Pid} -> exit(Pid,shutdown);
        error    -> ?LOG_UNKNOWN_AGENT(Agent_Id, State#state.agents)
    end,
    {noreply, State};
handle_cast({score, Agent_Id, Score} = Message, State) ->
    ?LOG_HANDLE_CAST_MESSAGE(Message, State),
    Agents     = State#state.agents,
    UpdtAgents = case maps:get(Agent_Id, Agents, error) of
        error -> ?LOG_UNKNOWN_AGENT(Agent_Id, Agents), Agents;
        Value -> score_agent(Agent_Id, Value, Score, Agents)
    end,
    {noreply, State#state{
        agents = UpdtAgents
    }};

    

handle_cast(Request, State) ->
    ?LOG_WARNING(#{what=>"Unknown handle_cast request", pid=>self(),
                   details => #{request => Request}}),
    {noreply, State}.



find_agent(Id, #state{agents:=#{Id:=Info}} = State) -> 
    Info;
find_agent(Id, State) -> ?LOG_REQUEST_FOR_UNKNOWN_AGENT(Id, State).


add_score_agent(Id, Score, #state{agents:=#{Id:=Info}} = State) ->
    {ActualScore, Pid} = Info,
    NewScore = ActualScore + Score,
    handle_score(Id, NewScore, State),
    maps:update(Id, {NewScore, Pid}, State#state.agents);
add_score_agent(Id, _, State) -> 
    ?LOG_UNKNOWN_AGENT(Id, State),
    State#state.agents.



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
-spec handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}.

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
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #state{}) -> term().
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
-spec code_change(OldVsn :: term() | {down, term()}, State :: #state{},
                  Extra :: term()) ->
                     {ok, NewState :: #state{}} | {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


% --------------------------------------------------------------------
start_agent(Agent_Id, State) -> 
    {ok, Pid} = agents_sup:start_agent(get(sup), get(id), Agent_Id),
    Ref       = erlang:monitor(process, Pid),
    {      Size,      Max_Size} = State#state.size,
    {Generation, MaxGeneration} = State#state.generation,
    Agents                      = State#state.agents,
    References                  = State#state.refs,
    State#state{
        size       = {      Size+1,      Max_Size},
        generation = {Generation+1, MaxGeneration},
        agents = Agents#{Agent_Id => {?INIT_SCORE, Pid}},
        refs   = References#{Ref => Agent_Id}
    }.

% --------------------------------------------------------------------
score_agent(Agent_Id, {OldScore, Pid}, Score, Agents) -> 
    NewScore = OldScore + Score,
    Score_Pool = get(spool),
    true = ets:delete(Score_Pool, {OldScore, Agent_Id}),
    true = ets:insert(Score_Pool, {NewScore, Agent_Id}),
    maps:update(Agent_Id, {NewScore, Pid}, Agents).
    







% --------------------------------------------------------------------
update_agent(Id, Score, Agents) ->
    {
    handle_score(Id, NewScore, State),
    maps:update(Id, {NewScore, Pid}, State#state.agents);
add_score_agent(Id, _, State) -> 
    ?LOG_UNKNOWN_AGENT(Id, State),
    State#state.agents.



% --------------------------------------------------------------------
handle_score(Agent_Id, Score+OldScore, State) -> 




handle_set_score(A_Id, NewScore, OldScore, S_Info, #state{score_limit = Limit} = State) when NewScore >= Limit ->
    self() ! run_end,
    handle_set_score(A_Id, NewScore, OldScore, S_Info, State#state{score_limit = alert_raised});
handle_set_score(Agent_Id, NewScore, OldScore, Score_Info, State) ->
    #state{agents = Agents, score_pool = ScorePool} = State,
    ets:delete(ScorePool, {OldScore, Agent_Id}),
    ets:insert(ScorePool, {{NewScore, Agent_Id}, Score_Info}),
    State#state{agents = orddict:store(Agent_Id, NewScore, Agents)}.



% --------------------------------------------------------------------
update_population(State) -> 
    true = ets:update_element(?EV_POOL, get(id), [
        {      size,       State#state.size},
        {  run_time,   State#state.run_time},
        {generation, State#state.generation},
        {best_score, State#state.best_score}
    ]).

% --------------------------------------------------------------------
clean_dead(State) -> 
    To_Clean = put(agents, maps:keys(State#state.agents)),
    clean_dead(To_Clean, State).

clean_dead([Id | Rest], State) -> 
    Agents   = State#state.agents,
    NewState = case maps:get(Id, Agents) of
        {_,dead} -> State;
        {_,   _} -> State#state{agents=maps:remove(Id, Agents)}
    end,
    clean_dead(Rest, NewState);
clean_dead([], State) -> State.















% --------------------------------------------------------------------
population_result(State) ->
    _Result = #{
        best_score   => ets:last(State#state.score_pool),
        n_agents     => State#state.agents_counter,
        running_time => (erlang:monotonic_time(millisecond) - State#state.start_time)}.

% --------------------------------------------------------------------..................................................
handle_run(Agent_Id, Population_Id, Agents_Sup) ->
    {ok, PId} = agents_sup:start_agent(Agents_Sup, Population_Id, Agent_Id),
    {erlang:monitor(process, PId), PId}.

% --------------------------------------------------------------------..................................................
handle_queue(Out, #state{agents_counter = Counter, spawn_limit = Limit} = State) when Counter >= Limit ->
    self() ! run_end,
    handle_queue(Out, State#state{
        spawn_limit = alert_raised
    });
handle_queue({value, {reply, From, Agent_Id}}, State) ->
    {Ref, PId} = handle_run(Agent_Id, State#state.id, State#state.agents_sup),
    gen_server:reply(From, {ok, PId}),
    State#state{
        agents = orddict:store(Agent_Id, ?INIT_AGENT_SCORE, State#state.agents),
        refs   = gb_trees:insert(Ref, Agent_Id, State#state.refs)
    };
handle_queue({value, {noreply, Agent_Id}}, State) ->
    {Ref, _PId} = handle_run(Agent_Id, State#state.id, State#state.agents_sup),
    State#state{
        agents = orddict:store(Agent_Id, ?INIT_AGENT_SCORE, State#state.agents),
        refs   = gb_trees:insert(Ref, Agent_Id, State#state.refs)
    };
handle_queue(empty, State) ->
    handle_evolution(State).

% --------------------------------------------------------------------..................................................
handle_set_score(A_Id, NewScore, OldScore, S_Info, #state{score_limit = Limit} = State) when NewScore >= Limit ->
    self() ! run_end,
    handle_set_score(A_Id, NewScore, OldScore, S_Info, State#state{score_limit = alert_raised});
handle_set_score(Agent_Id, NewScore, OldScore, Score_Info, State) ->
    #state{agents = Agents, score_pool = ScorePool} = State,
    ets:delete(ScorePool, {OldScore, Agent_Id}),
    ets:insert(ScorePool, {{NewScore, Agent_Id}, Score_Info}),
    State#state{agents = orddict:store(Agent_Id, NewScore, Agents)}.

% --------------------------------------------------------------------..................................................
scoreAgents(Pool) -> scoreAgents(Pool, ets:last(Pool)).

scoreAgents(Pool, {_Score, _Agent_Id} = Key) -> [Key | scoreAgents(Pool, ets:prev(Pool, Key))];
scoreAgents(_Pool, '$end_of_table')          -> [].

% --------------------------------------------------------------------..................................................
handle_evolution(State) ->
    #state{limit      = Limit,
           score_pool = ScorePool,
           agents     = Agents,
           evo_alg_f  = EvolutionF,
           sel_alg_f  = SelectionF} = State,
    ScoreAgents_List = scoreAgents(ScorePool),
    NClonesToGenerate = EvolutionF(Limit, length(Agents)),
    AgentsToMutate = SelectionF(ScoreAgents_List, NClonesToGenerate),
    ListOfNewAgents = [eevo:mutate(Agent_Id) || Agent_Id <- AgentsToMutate],
    _NewState = lists:foldl(fun(A_Id, S) -> handle_run_agent(A_Id, S) end, State, ListOfNewAgents).

handle_run_agent(Agent_Id, #state{limit = L, agents = Agents} = State) when L > length(Agents) ->
    {Ref, _PId} = handle_run(Agent_Id, State#state.id, State#state.agents_sup),
    State#state{
        agents = orddict:store(Agent_Id, ?INIT_AGENT_SCORE, Agents),
        refs   = gb_trees:insert(Ref, Agent_Id, State#state.refs)};
handle_run_agent(Agent_Id, State) ->
    State#state{
        queue = queue:in(Agent_Id, State#state.queue)}.

% --------------------------------------------------------------------..................................................
report_agent(Agent_Id, Score, Pool) ->
    [{{Score, Agent_Id}, Additional_Info}] = ets:lookup(Pool, {Score, Agent_Id}),
    Agent = edb:read(Agent_Id),
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

