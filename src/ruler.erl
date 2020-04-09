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

-behaviour(gen_statem).

%% API
%%-export([start_link/0]).
-export_type([id/0, property/0, properties/0]).

%% gen_statem callbacks
-export([init/1, format_status/2, handle_event/4, terminate/3, 
         code_change/4, callback_mode/0]).

-type id() :: {Ref :: reference(), ruler}.
-type property()   :: id | max_size | stop_time | generations | 
                      target | selection.
-type properties() :: #{
    OptionalProperty :: property() => Value :: term()
}.

-define(MAX_SIZE_TO_SELECTION,    20).
-define(POOL_UPDATE_INTERVAL,     10).
-define(CLEAN_DEAD_INTERVAL,      90).
-define(RUNTIME_UPDATE_INTERVAL,   2).
-define(ETS_TABLE_OPTIONS, [ordered_set]).
-define(INIT_SCORE, 0.0).

-record(s, { % Short state record 
    size       :: {Current :: integer(), Max :: integer()},
    runtime    :: {Current :: integer(), End :: integer()},
    generation :: {Current :: integer(), End :: integer()}, 
    score      :: {Current ::   float(), End ::   float()},   
    selection  :: selection:func(),
    agents     :: #{Agent_Id :: agent:id() => Score :: number()},
    queue      :: queue:queua(),
    from       :: {Pid ::pid(), Ref :: reference()}
 }).

-define(LOG_STATE_CHANGE(OldState, NewState),
    ?LOG_INFO(#{what => "Ruler state has changed", 
                pid=>self(), id => get(id), details => #{
                    state_new=>NewState, old_state=>OldState}},
              #{logger_formatter=>#{title=>"RULER STATE"}})
).
-define(LOG_RUN_REQUEST_RECEIVED,
    ?LOG_DEBUG(#{what => "Received run request", pid=>self(),
                 details => #{}},
               #{logger_formatter=>#{title=>"RULER REQUEST"}})
).
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
-define(LOG_SCORE_REQUEST_RECEIVED(Id, Score),
    ?LOG_DEBUG(#{what => "Received cast score request", pid=>self(),
                 details => #{id=>Id, score=>Score}},
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
-spec start_link(Ruler_Id :: id()) -> gen_statem:start_ret().
start_link(Ruler_Id) ->
    gen_statem:start_link(?MODULE, [Ruler_Id, self()], []).

%%--------------------------------------------------------------------
%% @doc Request the ruler to run the population.
%% @end
%%--------------------------------------------------------------------
-spec run(Ruler :: pid()) -> ok.
run(Ruler) -> 
    gen_statem:call(Ruler, run).

%%--------------------------------------------------------------------
%% @doc Request the agent addition to the population. Asynchronous.
%% @end
%%--------------------------------------------------------------------
-spec async_queue(Ruler :: pid(), Agent_Id :: agent:id()) ->
    ok.
async_queue(Ruler, Agent_Id) ->
    gen_statem:cast(Ruler, {queue, Agent_Id}).

%%--------------------------------------------------------------------
%% @doc Request the kill of an agent. Asynchronous.
%% @end
%%--------------------------------------------------------------------
-spec async_kill(Ruler :: pid(), Agent_Id :: agent:id()) ->
    ok.
async_kill(Ruler, Agent_Id) ->
    gen_statem:cast(Ruler, {kill, Agent_Id}).

%%--------------------------------------------------------------------
%% @doc Adds a score to an agent. Asynchronous.
%% @end
%%--------------------------------------------------------------------
-spec score(Ruler, Agent_Id, Score) -> ok when 
    Ruler    :: pid(), 
    Agent_Id :: agent:id(), 
    Score    :: float().
score(Ruler, Agent_Id, Score) ->
    gen_statem:cast(Ruler, {score, Agent_Id, Score}).

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
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
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
    self() ! update_population, % Request to update pool after start
    {RunnedTime, EndTime} = demography:runtime(Ruler),
    timer:send_after(       EndTime - RunnedTime,       runtime_end),
    timer:send_interval(   ?POOL_UPDATE_INTERVAL, update_population),
    timer:send_interval(    ?CLEAN_DEAD_INTERVAL,        clean_dead),
    timer:send_interval(?RUNTIME_UPDATE_INTERVAL,     eval_runtime),
    {ok, stopped, #s{
        size       = {  0,    demography:max_size(Ruler)},
        runtime    = demography:runtime(Ruler),
        generation = demography:generation(Ruler), 
        score      = demography:score(Ruler),   
        selection  = demography:selection(Ruler),
        agents     = #{},
        queue      = queue:new()
    }}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it needs to find out 
%% the callback mode of the callback module.
%%
%% @spec callback_mode() -> atom().
%% @end
%%--------------------------------------------------------------------
callback_mode() ->
    [
        handle_event_function,
        state_enter
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
%%
%% @spec format_status(Opt, [PDict, StateName, State]) -> term()
%% @end
%%--------------------------------------------------------------------
format_status(_Opt, [_PDict, _StateName, _State]) ->
    Status = some_term,
    Status.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Actions} |
%%                   {stop, Reason, NewState} |
%%                     stop |
%%                   {stop, Reason :: term()} |
%%                   {stop, Reason :: term(), NewData :: data()} |
%%                   {stop_and_reply, Reason, Replies} |
%%                   {stop_and_reply, Reason, Replies, NewState} |
%%                   {keep_state, NewData :: data()} |
%%                   {keep_state, NewState, Actions} |
%%                   keep_state_and_data |
%%                   {keep_state_and_data, Actions}
%% @end
%%--------------------------------------------------------------------
%% Enter events
%%--------------------------------------------------------------------
handle_event(enter, OldState, running, State) ->
    ?LOG_STATE_CHANGE(OldState, running),
    {next_state, running, State};
handle_event(enter, running, stopped, State) ->
    ?LOG_STATE_CHANGE(running, stopped),
    [pop_sup:stop_agent(Id,get(sup))||Id<-maps:keys(State#s.agents)],
    print_stop_report(State),
    save_population(State),
    {next_state, stopped, State, {reply, State#s.from, ok}};
handle_event(enter, stopped, stopped, State) ->
    ?LOG_STATE_CHANGE(stopped, stopped),
    {next_state, stopped, State};
%%--------------------------------------------------------------------
%% Call requests
%%--------------------------------------------------------------------
handle_event({call, From}, run, stopped, State) ->
    ?LOG_RUN_REQUEST_RECEIVED,
    {0, Max} = State#s.size, 
    {next_state, running, State#s{from = From},
        [{next_event, internal, new} || _ <- lists:seq(1, Max)]};
%%--------------------------------------------------------------------
%% Cast requests
%%--------------------------------------------------------------------
handle_event(cast, {queue, Id}, StateName, State) ->
    ?LOG_QUEUE_REQUEST_RECEIVED(Id, State#s.queue),
    handle_event(internal, new, StateName, State#s{
        queue = queue:in(Id, State#s.queue)
    });
handle_event(cast,  {kill, Id},_StateName, State) ->
    ?LOG_KILL_REQUEST_RECEIVED(Id, State#s.agents),
    case pop_sup:stop_agent(Id, get(sup))of
        ok                 -> keep_state_and_data;
        {error, not_found} -> keep_state_and_data
    end;
handle_event(cast, {score, Id, Score}, StateName, State) ->
    ?LOG_SCORE_REQUEST_RECEIVED(Id, Score),
    case maps:get(Id, State#s.agents, error) of
        error    -> 
            handle_event(internal, {unknown, Id}, StateName, State);
        OldScore -> 
            NS = OldScore + Score,
            update_pool(Id, OldScore, NS),
            handle_event(internal,{eval_score,NS},StateName,State#s{
                agents = maps:update(Id, NS, State#s.agents)
            })
    end;
%%--------------------------------------------------------------------
%% Update of information
%%--------------------------------------------------------------------
handle_event(info, update_population,_StateName, State) ->
    update_population(State),
    {keep_state, State};
handle_event(info,        clean_dead,_StateName, State) ->
    {keep_state, State#s{agents = clean_dead(State#s.agents)}};
handle_event(info, {'DOWN',Ref,process,Pid,_}, StateName, State) ->
    ?LOG_AGENT_DOWN(Ref, Pid),
    {Size, Max_Size} = State#s.size,
    handle_event(internal, eval_generation, StateName, State#s{
        size = {Size-1, Max_Size}
    });
%%--------------------------------------------------------------------
%% New agents
%%--------------------------------------------------------------------
handle_event(internal, new, running,#s{size={S,M}}=State) when S<M ->
    case queue:out(State#s.queue) of
        {{value,Id}, Queue} -> Id = run_agent(Id);
        {     empty, Queue} -> Id = run_mutated(State)
    end,
    {Generation, MaxGeneration} = State#s.generation,    
    {keep_state, State#s{
        size       = {         S+1,             M},
        generation = {Generation+1, MaxGeneration},
        agents     = maps:put(Id, ?INIT_SCORE, State#s.agents),
        queue      = Queue
    }};
handle_event(internal, new,_StateName, State) ->
    {keep_state, State};
%%--------------------------------------------------------------------
%% Evaluation of new report results
%%--------------------------------------------------------------------
handle_event(info,     eval_runtime,_StateName, State) ->
    Elapsed = erlang:monotonic_time(millisecond) - get(start_time),
    {_, Runtime_End} = State#s.runtime,
    {keep_state, State#s{runtime = {Elapsed, Runtime_End}}};
handle_event(internal, eval_generation, running, State) ->
    case State#s.generation of
        {N, Max} when N< Max -> 
            handle_event(internal, new, running, State);
        {N, Max} when N>=Max -> 
            handle_event(internal, last_generation, running, State)
    end;
handle_event(internal, eval_generation,  stopped, State) ->
    {keep_state, State};
handle_event(internal, {eval_score,S}, StateName, State) ->
    case State#s.score of
        {B,_} when S<B -> 
            {keep_state, State};
        {_,T} when S<T -> 
            {keep_state, State#s{score={S,T}}};
        {_,T} -> 
            handle_event(internal, score_reached, 
                         StateName, State#s{score={S,T}})
    end;
%%--------------------------------------------------------------------
%% Events which would stop the population
%%--------------------------------------------------------------------
handle_event(info,         runtime_end, running, State) -> 
    {next_state, stopped, State};
handle_event(internal, last_generation, running, State) ->
    {next_state, stopped, State};
handle_event(internal,   score_reached, running, State) ->
    {next_state, stopped, State};
%%--------------------------------------------------------------------
%% Unknown agents and events
%%--------------------------------------------------------------------
handle_event(internal, {unknown, Id},_StateName, State) ->
    ?LOG_UNKNOWN_AGENT(Id, State#s.agents),
    {keep_state, State};
handle_event(EventType, EventContent, StateName, State) ->
    ?LOG_WARNING(#{what => "Unknown event", pid=>self(),
                   details => #{
                       state  => StateName, 
                       type   => EventType,
                       content=> EventContent 
    }}),
    {keep_state, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason, OldState, _State) ->
    ?LOG_INFO(#{what => "Ruler terminating", pid => self(), 
                details => #{reason => Reason, state => OldState}}),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

% --------------------------------------------------------------------
run_agent(Id) -> 
    {ok, Pid} = pop_sup:start_agent(Id, get(id), get(sup)),
    erlang:monitor(process, Pid),
    Id.

% --------------------------------------------------------------------
run_mutated(State) -> 
    TopScoreAgents = top(get(spool), ?MAX_SIZE_TO_SELECTION),
    SelectedId = selection:func(State#s.selection, TopScoreAgents),
    Id = eevo:mutate(SelectedId),
    run_agent(Id).

% --------------------------------------------------------------------
update_pool(Id, OldScore, NewScore) -> 
    Score_Pool = get(spool),
    true = ets:delete(Score_Pool, {OldScore, Id}),
    true = ets:insert(Score_Pool, {{NewScore, Id}}).

% --------------------------------------------------------------------
update_population(State) -> 
    true = ets:update_element(?EV_POOL, get(id), [
        {      #population.size,       State#s.size},
        {   #population.runtime,    State#s.runtime},
        {#population.generation, State#s.generation},
        {     #population.score,      State#s.score}
    ]).

% --------------------------------------------------------------------
clean_dead(Agents) -> 
    Previous = put(agents, maps:keys(Agents)),
    Running  = [Id||{Id,_,_,_}<-supervisor:which_children(get(sup))],
    clean_dead(Previous -- Running, Agents).

clean_dead([Id | Rest], Agents) -> 
    clean_dead(Rest, maps:remove(Id, Agents));
clean_dead([], Agents) -> Agents.

% --------------------------------------------------------------------
print_stop_report(State) -> 
    io:format([
        "Training stopped \n",
        io_lib:format("\tRuning time:\t~p\n", [State#s.runtime]),
        io_lib:format("\tGenerations:\t~p\n", [State#s.generation]),
        io_lib:format("\tBest score:\t~p\n",  [State#s.score]),
        io_lib:format("\ttop3 agents:\t~p\n", [top(get(spool), 3)])
    ]).

% --------------------------------------------------------------------
save_population(State) -> 
    Id         = get(id),
    Population = demography:edit(edb:read(Id), #{
        max_size   => element(2, State#s.size),
        runtime    => State#s.runtime,
        generation => State#s.generation,
        score      => State#s.score,
        champion   => element(2, ets:last(get(spool))),
        selection  => State#s.selection
    }),
    edb:write(Population).
    

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
