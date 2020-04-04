%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(test_agent).
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

-include_lib("kernel/include/logger.hrl").
-include_lib("society.hrl").

-behaviour(gen_server). %TODO: Develop as gen_agent

%% Defined agent species
-export([properties_example/0]).

%% Mutation functions
-export([mutationF_example/1]).

%% gen_server callbacks
-export([init/1, handle_info/2, terminate/2]).

% hidden functions (for compatibility, do not modify)
-export([start_link/3]).
-export([handle_call/3, handle_cast/2, code_change/3]).

-define(AGENT, ?MODULE).

-record(state, {
    id :: agent_id(),
    properties :: #{}}).

-ifdef(debug_mode).
-define(STDCALL_TIMEOUT, infinity).
-else.
-define(STDCALL_TIMEOUT, 5000).
-endif.

%%%===================================================================
%%% Defined agents
%%%===================================================================

properties_example() ->
    _AgentProperties = #{
        % Extension of properties should be placed here
        score_base => rand:uniform(100)}.

mutationF_example(Properties) ->
    #{score_base := Base} = Properties,
    _ChildProperties = Properties#{
        score_base := Base + rand:uniform(10)}.

%%%===================================================================
%%% agent callbacks as gen_server
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the agent
%%
%% Example: In this example, the agent creates a timer to receive
%% after a random time the order to score.
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec agent_init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
agent_init([Properties]) ->
    process_flag(trap_exit, true), % Mandatory to catch supervisor exits
    timer:send_after(rand:uniform(10) + 5, score),
    {ok, #state{
        id         = ?agent_id(Properties),
        properties = Properties#{agent_pid => self()}
    }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages sent to the agent
%%
%% Example: In this example, the agent receives a score message to
%% indicate it is the moment to score on the population.
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).

handle_info(score, State) ->
    ?LOG_INFO("handle_info --> score Agent_Id ~p", [State#state.id]),
    ?add_score(State#state.properties, maps:get(score_base, State#state.properties) + rand:uniform(25)),
    ?end_agent(State#state.properties),
    {noreply, State};

handle_info({'EXIT', _PId, Reason}, State) ->
    ?LOG_INFO("handle_info --> Agent_Id ~p 'EXIT', Reason ~p", [State#state.id, Reason]),
    case Reason of
        end_agent -> {stop, normal, State};
        time_end -> {stop, normal, State};
        _Other -> ?LOG_ERROR({"'EXIT' message with Reason: ", Reason}), {stop, Reason, State}
    end;

handle_info(Info, State) ->
    ?LOG_WARNING("handle_info --> Agent_Id ~p received and unknown Info message ~p", [State#state.id, Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_agent when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% To catch supervisor shutdowns the agent must trap exit signals
%%
%% Example: No end function is needed in this example.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #state{}) -> term()).
terminate(_Reason, _State) ->
%%    io:format("Terminate ~p ~n", [Reason]),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%%%===================================================================
%%% Static functions (Do not modify)
%%%===================================================================
% This is done like this to save time on implementing a specific behaviour "Agent"
start_link(Agent_Id, Population_Id, Properties) ->
    Updt_Properties = Properties#{
        population_id  => Population_Id,
        agent_id       => Agent_Id
    },
    gen_server:start_link(?MODULE, [Updt_Properties], []).

init([Properties]) -> agent_init([Properties#{agent_pid => self()}]).
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Request, State) -> {noreply, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.



