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
-export_type([id/0, property/0, properties/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-type id() :: {Ref :: reference(), ruler}.
-type property()   :: id | max_size | stop_time | generations | 
                      target | selection.
-type properties() :: #{
    OptionalProperty :: property() => Value :: term()
}.

-define(MAX_SIZE_TO_SELECTION,     20).
-define(POOL_UPDATE_INTERVAL,     100).
-define(CLEAN_DEAD_INTERVAL,       90).
-define(RUNTIME_UPDATE_INTERVAL,   25).
-define(ETS_TABLE_OPTIONS, [ordered_set]).
-define(INIT_SCORE, 0.0).

-record(s, { % Short state record 
    size       :: {Current :: integer(), Max :: integer()},
    run_time   :: {Current :: integer(), End :: integer()},
    generation :: {Current :: integer(), End :: integer()}, 
    best_score :: {Current ::   float(), End ::   float()},   
    selection  :: selection:func(),
    agents     :: #{Agent_Id  :: agent:id()  => Info :: info()},
    queue      :: queue:queua()
 }).
-type info() :: {Score :: float(), Pid :: pid()}.

-define(LOG_QUEUE_REQUEST_RECEIVED(Id, Queue),
    ?LOG_DEBUG(#{what => "Received cast queue request", pid=>self(),
                 details => #{id => Id, queue => Queue}},
               #{logger_formatter=>#{title=>"RULER REQUEST"}})
).
-define(LOG_KILL_REQUEST_RECEIVED(Id, Agents),
    ?LOG_DEBUG(#{what => "Received cast kill request", pid=>self(),
                 details => #{id => Id, agents => Agents}},
               #{logger_formatter=>#{title=>"RULER REQUEST"}})
).
-define(LOG_SCORE_REQUEST_RECEIVED(Id, Score, Agents),
    ?LOG_DEBUG(#{what => "Received cast score request", pid=>self(),
                 details => #{id=>Id, score=>Score, agents=>Agents}},
               #{logger_formatter=>#{title=>"RULER REQUEST"}})
).
-define(LOG_AGENT_DOWN(DownRef, Pid),
    ?LOG_DEBUG(#{what => "Agent down", pid=>self(),
                 details => #{ref => DownRef, pid => Pid}},
               #{logger_formatter=>#{title=>"RULER REQUEST"}})
).
-define(LOG_UNKNOWN_AGENT(Agent_Id, Agents),
    ?LOG_DEBUG(#{what => "Request for unknow agent", pid => self(),
                 details => #{agent_id=>Agent_Id, agents=>Agents}},
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
%% @doc Creates a new ruler and stores it on the database.  
%% @end
%%--------------------------------------------------------------------
-spec new(Properties) -> id() when
    Properties :: properties().
new(Properties) ->
    Ruler = demography:ruler(Properties),
    edb:write(Ruler),
    demography:id(Ruler).

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

%%--------------------------------------------------------------------
%% @doc Returns the top N of an score pool in a format {Id, Score}.
%% @end
%%--------------------------------------------------------------------
-spec top(Score_Pool :: ets:tid(), N :: integer()) -> 
    [{Agent_Id :: agent:id(), Score :: float()}].
top(Pool, N) -> last_n(Pool, ets:last(Pool), N).

last_n(_Pool, '$end_of_table',_N)       -> [];
last_n( Pool,      ScoreAgent, N) when N > 0 ->
    [ScoreAgent|last_n(Pool, ets:prev(Pool,ScoreAgent), N-1)];
last_n(_Pool,     _ScoreAgent,_N)       -> [].

%%--------------------------------------------------------------------
%% @doc Returns the N agents with the lowest score.
%% @end
%%--------------------------------------------------------------------
-spec bottom(Score_Pool :: ets:tid(), N :: integer()) -> 
    [{Agent_Id :: agent:id(), Score :: float()}].
bottom(Pool, N) -> first_n(Pool, ets:first(Pool), N).

first_n(_Pool, '$end_of_table',_N)            -> [];
first_n( Pool,      ScoreAgent, N) when N > 0 ->
    [ScoreAgent|first_n(Pool,ets:next(Pool,ScoreAgent), N-1)];
first_n(_Pool,     _ScoreAgent,_N)            -> [].


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_server is started using gen_server:start/[3,4] or
%% gen_server:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {CallbackMode, StateName, State} |
%%                     {CallbackMode, StateName, State, Actions} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([Id, Supervisor]) ->
    Ruler      = edb:read(Id),    
    Score_Pool = ets:new(undef, ?ETS_TABLE_OPTIONS),
    put(   id,          Id), % Used when updating the eevo_pool
    put(   sup, Supervisor), % Used when spawn agents under the OTP
    put(agents,         []), % Used when cleaning dead from agents#{}
    put( spool, Score_Pool), % Used when scoring agents
    put(start_time, erlang:monotonic_time(millisecond)), 
    true = ets:update_element(?EV_POOL, Id, [
        {     #population.ruler,     self()},
        {#population.score_pool, Score_Pool}
    ]),
    process_flag(trap_exit, true),
    ?LOG_INFO("Population_Id:~p initiated", [Id]),
    self() ! update_pool, % Request to update pool after start
    timer:send_interval(   ?POOL_UPDATE_INTERVAL, update_population),
    timer:send_interval(    ?CLEAN_DEAD_INTERVAL,        clean_dead),
    timer:send_interval(?RUNTIME_UPDATE_INTERVAL,    update_runtime),
    timer:send_after(demography:stop_time(Ruler),       runtime_end),
    {ok, #s{
        size       = {  0,    demography:max_size(Ruler)},
        run_time   = {  0,   demography:stop_time(Ruler)},
        generation = {  0, demography:generations(Ruler)}, 
        best_score = {0.0,      demography:target(Ruler)},   
        selection  = demography:selection(Ruler),
        agents     = #{},
        queue      = queue:new()
    }}.


%%--------------------------------------------------------------------
%% @private
%% @doc Handling call messages
%% @spec handle_call(Request :: term(), From :: {pid(), Tag :: term()},
%%                   State :: #state{}) ->
%%                      {reply, Reply :: term(), NewState :: #state{}} |
%%                      {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
%%                      {noreply, NewState :: #state{}} |
%%                      {noreply, NewState :: #state{}, timeout() | hibernate} |
%%                      {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
%%                      {stop, Reason :: term(), NewState :: #state{}}.
%% handle_call(Request, _From, State) ->
%%     ?LOG_WARNING("Unknown handle_call Population_Id ~p, request ~p", 
%%                  [get(id), Request]),
%%     {reply, {error, badrequest}, State}
%% @end
%%--------------------------------------------------------------------
handle_call(Request, _From, State) ->
    ?LOG_WARNING(#{what=>"Unknown handle_call", pid=>self(),
                   details => #{request => Request}}),
    {reply, {error, badrequest}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec(handle_cast(Request :: term(), State :: #state{}) ->
%%     {noreply, NewState :: #state{}} |
%%     {noreply, NewState :: #state{}, timeout() | hibernate} |
%%     {stop, Reason :: term(), NewState :: #state{}}).
%% @end
%%--------------------------------------------------------------------
handle_cast({queue, Id}, State) ->
    ?LOG_QUEUE_REQUEST_RECEIVED(Id, State#s.queue),
    {noreply, State#s{
        queue = queue:in(Id, State#s.queue)
    }};
handle_cast({kill, Id}, State) ->
    ?LOG_KILL_REQUEST_RECEIVED(Id, State#s.agents),
    case maps:get(Id, State#s.agents, error) of
        {_, Pid} -> 
            exit(Pid, shutdown), 
            {noreply, State};
        error    -> 
            unknown_agent(Id, State)
    end;
handle_cast({score, Id, Score}, State) ->
    ?LOG_SCORE_REQUEST_RECEIVED(Id, Score, State#s.agents),
    Agents = State#s.agents,
    case maps:get(Id, Agents, error) of
        {OldScore, Pid} ->
            score_agent(Id, OldScore, Pid, Score, State);
        error           -> 
            unknown_agent(Id, State)
    end;
handle_cast(Request, State) ->
    ?LOG_WARNING(#{what=>"Unknown handle_cast", pid=>self(),
                   details => #{request => Request}}),
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
handle_info({'DOWN', DownRef, process, Pid, _}, State) ->
    ?LOG_AGENT_DOWN(DownRef, Pid),
    {Size, Max_Size} = State#s.size,
    eval_generation(State#s{
        size = {Size-1, Max_Size}
    });
handle_info(update_runtime, State) ->
    Elapsed = erlang:monotonic_time(millisecond) - get(start_time),
    {_, Runtime_End} = State#s.run_time,
    {noreply, State#s{
        run_time = {Elapsed, Runtime_End}
    }};
handle_info(update_population, State) ->
    update_population(State),
    {noreply, State};
handle_info(clean_dead, State) ->
    {noreply, State#s{
        agents = clean_dead(State#s.agents)
    }};
handle_info(runtime_end, State) ->
    {stop, runtime_end, State};    
handle_info(Info, State) ->
    ?LOG_WARNING(#{what=>"Unknown handle_info", pid=>self(),
                   details => #{info  => Info}}),
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
                State :: #s{}) -> term().
terminate(Reason, _State) ->
    ?LOG_INFO(#{what => "Ruler terminating", pid => self(), 
                details => #{reason => Reason}}),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()}, State :: #s{},
                  Extra :: term()) ->
                     {ok, NewState :: #s{}} | {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

% --------------------------------------------------------------------
eval_generation(State) ->
    case State#s.generation of
        {N, Max} when N >= Max -> {stop, last_generation, State};
        {N, Max} when N <  Max -> start_agent(State)
    end.

% --------------------------------------------------------------------
eval_best(Score, State) ->
    case State#s.best_score of
        {_,Target} when Score>=Target -> {stop,score_reached,State};
        {Best,_}   when Score< Best   -> {noreply, State};
        {_,Target} -> 
            {noreply, State#s{best_score = {Score, Target}}}
    end.

% --------------------------------------------------------------------
start_agent(State) ->
    case queue:out(State#s.queue) of
        {{value,Id}, Queue} -> Id = run_agent(Id);
        {     empty, Queue} -> Id = run_mutated(State)
    end,
    {      Size,      Max_Size} = State#s.size,
    {Generation, MaxGeneration} = State#s.generation,    
    {noreply, State#s{
        size       = {      Size+1,      Max_Size},
        generation = {Generation+1, MaxGeneration},
        agents     = maps:put(Id, ?INIT_SCORE, State#s.agents),
        queue      = Queue
    }}.

% --------------------------------------------------------------------
run_agent(Id) -> 
    {ok, Pid} = pop_sup:start_agent(get(sup), Id),
    erlang:monitor(process, Pid),
    Id.

% --------------------------------------------------------------------
run_mutated(State) -> 
    TopScoreAgents = top(get(spool), ?MAX_SIZE_TO_SELECTION),
    SelectedId = selection:func(State#s.selection, TopScoreAgents),
    Id = eevo:mutate(SelectedId),
    run_agent(Id).

% --------------------------------------------------------------------
score_agent(Id, OldScore, Pid, Score, State) -> 
    NewScore = OldScore + Score, 
    update_pool(Id, OldScore, NewScore),
    eval_best(NewScore, State#s{
        agents = maps:update(Id, {NewScore, Pid}, State#s.agents)
    }).

% --------------------------------------------------------------------
update_pool(Id, OldScore, NewScore) -> 
    Score_Pool = get(spool),
    true = ets:delete(Score_Pool, {OldScore, Id}),
    true = ets:insert(Score_Pool, {{NewScore, Id}}).

% --------------------------------------------------------------------
unknown_agent(Id, State) -> 
    ?LOG_UNKNOWN_AGENT(Id, State#s.agents),
    {noreply, State}.

% --------------------------------------------------------------------
update_population(State) -> 
    true = ets:update_element(?EV_POOL, get(id), [
        {      size,       State#s.size},
        {  run_time,   State#s.run_time},
        {generation, State#s.generation},
        {best_score, State#s.best_score}
    ]).

% --------------------------------------------------------------------
clean_dead(Agents) -> 
    To_Clean = put(agents, maps:keys(Agents)),
    clean_dead(To_Clean, Agents).

clean_dead([Id | Rest], Agents) -> 
    {_, Pid} = maps:get(Id, Agents),
    Agents = case is_process_alive(Pid) of
        true  -> clean_dead(Rest, Agents);
        false -> clean_dead(maps:remove(Id, Agents))
    end;
clean_dead([], Agents) -> Agents.


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
