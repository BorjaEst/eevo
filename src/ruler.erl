%%%-------------------------------------------------------------------
%%% @doc The ruler is the process in charge of spawning the population
%%% agents. Also evaluates the end condition to stop the population 
%%% and returns the results to the requester.
%%% @end
%%%-------------------------------------------------------------------
-module(ruler).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").

-behaviour(gen_statem).

%% API
-export([run/4]).
-export_type([]).

%% gen_statem callbacks
-export([init/1, format_status/2, handle_event/4, terminate/3, 
         code_change/4, callback_mode/0]).

-define(MAX_SIZE_TO_SELECTION,    20).
-define(RUNTIME_UPDATE_INTERVAL,   2).

-record(state, { 
    population_id       :: population:id(),
    supervisor          :: pid(),
    score_pool          :: scorer:pool(),
    score_group         :: scorer:group(),
    from                :: pid(),
    queue               :: queue:queue(),
    agents  = #{}       :: #{pid() => Id :: agent:id()},
    max_size            :: integer(),
    selection           :: selection:func(),
    stop_cond           :: function(),
    run_data            :: population:run_data()
 }).
-define(POPULATION_ID, State#state.population_id).
-define(   SUPERVISOR, State#state.supervisor   ).
-define(   SCORE_POOL, State#state.score_pool   ).
-define(  SCORE_GROUP, State#state.score_group  ).
-define(         FROM, State#state.from         ).
-define(        QUEUE, State#state.queue        ).
-define(       AGENTS, State#state.agents       ).
-define(         SIZE, map_size(?AGENTS)        ).
-define(     MAX_SIZE, State#state.max_size     ).
-define(    SELECTION, State#state.selection    ).
-define(    STOP_COND, State#state.stop_cond    ).
-define(         DATA, State#state.run_data     ).
-define(         TIME, erlang:monotonic_time(millisecond)).

-define(LOG_STATE_CHANGE(OldState, NewState),
    ?LOG_DEBUG(#{what => "Ruler state has changed", 
                pid=>self(), id => get(id), details => #{
                    state_new=>NewState, old_state=>OldState}},
              #{logger_formatter=>#{title=>"RULER STATE"}})
).
-define(LOG_NEW_CHAMPION(Id, Score),
    ?LOG_INFO(#{what => "New population best score", pid=>self(),
                 details => #{id=>Id, score=>Score}},
               #{logger_formatter=>#{title=>"RULER EVENT"}})
).
-define(LOG_AGENT_UP(Id, Pid),
    ?LOG_DEBUG(#{what => "Agent up", pid=>self(),
                 details => #{id => Id, pid => Pid}},
               #{logger_formatter=>#{title=>"RULER EVENT"}})
).
-define(LOG_AGENT_DOWN(DownRef, Pid),
    ?LOG_DEBUG(#{what => "Agent down", pid=>self(),
                 details => #{ref => DownRef, pid => Pid}},
               #{logger_formatter=>#{title=>"RULER EVENT"}})
).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts a ruler to run a population.
%% @end
%%--------------------------------------------------------------------
-spec run(Population_id, Agents_ids, Size, Stop_condition) -> ok when 
    Population_id  :: population:id(),
    Agents_ids     :: [agent:id()],
    Size           :: non_neg_integer(),
    Stop_condition :: function().
run(Population_id, Agents_ids, Size, Stop_condition) -> 
    Arg = [Population_id, Agents_ids, Size, Stop_condition],
    {ok, Pid} = gen_statem:start_link(?MODULE, [self()|Arg], []),
    receive {run_end, Pid} -> ok end.


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
init([From, Population_id, Agents_ids, Max_size, Stop_condition]) ->
    [Population]    = mnesia:dirty_read(population, Population_id),
    Population_info = population:info(Population),
    Score_table       = map_get(score_table, Population_info),
    {ok, Score_group} = scorer:new_group(),
    {ok,  Score_pool} = scorer:new_pool(Score_table, [Score_group]),
    ok = scorer:subscribe(Score_pool),
    timer:send_interval(?RUNTIME_UPDATE_INTERVAL, update_runtime),
    put(time, ?TIME), % To measure elapsed times
    ?LOG_INFO(#{what => "Ruler stating", pid => self(), 
                details => #{id=>Population_id, agents=>Agents_ids}}),
    {ok, running, #state{
        population_id = Population_id,
        supervisor    = eevo_sup:new_supervisor(Population_id),
        from          = From,
        stop_cond     = Stop_condition,
        score_group   = Score_group,
        score_pool    = Score_pool,
        selection     = map_get(selection, Population_info),
        run_data      = map_get(run_data, Population_info),
        max_size      = Max_size,
        queue = lists:foldl(fun queue:in/2, queue:new(), Agents_ids)
    }, [{next_event, internal, new} || _ <- lists:seq(1, Max_size)]}.


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
        handle_event_function
        % state_enter
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
%% New agents
%%--------------------------------------------------------------------
handle_event(internal, new, running, State) when ?SIZE < ?MAX_SIZE ->
    case queue:out(?QUEUE) of
        {{value,Id}, Queue} -> 
            {keep_state, State#state{queue = Queue}, 
                [{next_event, internal, {run, Id}}]};  
        {empty, _} -> 
            {keep_state_and_data, 
                [{next_event, internal, mutate_and_run}]}
    end;
handle_event(internal, new, _StateName, _State) ->
    keep_state_and_data;
handle_event(internal, mutate_and_run, running, State) ->
    case scorer:top(?SCORE_POOL, ?MAX_SIZE_TO_SELECTION) of 
        [_|_] = TopScoreAgents -> 
            SelectedId = selection:func(?SELECTION, TopScoreAgents),
            Id = eevo:mutate(SelectedId),
            {keep_state, State, [{next_event, internal, {run, Id}}]};    
        [] -> 
            keep_state_and_data
    end;
handle_event(internal, {run, Id}, running, State) ->
    {ok, Pid} = pop_sup:start_agent(?SUPERVISOR, ?SCORE_GROUP, Id),
    ?LOG_AGENT_UP(Id, Pid),
    erlang:monitor(process, Pid),
    true = agents_pool:register(Pid, Id, ?POPULATION_ID, ?SCORE_GROUP),
    {keep_state, State#state{
        agents = maps:put(Pid, Id, ?AGENTS)
    }}; 
%%--------------------------------------------------------------------
%% Update of information
%%--------------------------------------------------------------------
handle_event(info, {'DOWN',Ref,process,Pid,_}, _StateName, State) ->
    ?LOG_AGENT_DOWN(Ref, Pid),
    agents_pool:unregister(Pid),
    {keep_state, State#state{
        agents   = maps:remove(Pid, ?AGENTS),
        run_data = maps:update_with(generation, fun(N)-> N+1 end, ?DATA)
    }, [{next_event, internal, eval_stop}]};
handle_event(info, {new_best,_,{Id, Score}}, _StateName, State) ->
    ?LOG_NEW_CHAMPION(Id, Score),
    {keep_state, State#state{
        run_data = maps:update_with(best_score, fun(_)-> Score end, ?DATA)
    }, [{next_event, internal, eval_stop}]};
handle_event(info, update_runtime, _StateName, State) -> 
    Time_now = ?TIME,
    Tx = Time_now - put(time, Time_now),
    {keep_state, State#state{
        run_data = maps:update_with(runtime, fun(T)-> T+Tx end, ?DATA) 
    }, [{next_event, internal, eval_stop}]};
%%--------------------------------------------------------------------
%% Evaluation of population end 
%%--------------------------------------------------------------------
handle_event(internal, eval_stop, running, State) ->
    case apply(?STOP_COND, [?DATA]) of
        false -> {keep_state_and_data, [{next_event, internal, new}]};
        true  -> {stop, normal}
    end;
%%--------------------------------------------------------------------
%% Unknown agents and events
%%--------------------------------------------------------------------
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
terminate(Reason, OldState, State) ->
    ?LOG_INFO(#{what => "Ruler terminating", pid => self(), 
                details => #{reason => Reason, state => OldState}}),
    [pop_sup:stop_agent(?SUPERVISOR, Id) || Id<-maps:values(?AGENTS)],
    {atomic, ok} = mnesia:transaction(
        fun() ->
            [Population] = mnesia:read(population, ?POPULATION_ID), 
            mnesia:write(population:update(Population, ?DATA))
        end),
    ?FROM ! {run_end, self()}.

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

