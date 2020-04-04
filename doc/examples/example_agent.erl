%%%-------------------------------------------------------------------
%%% @author borja
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Sep 2018 12:25
%%%-------------------------------------------------------------------
-module(example_agent).
-compile(export_all).
-compile(nowarn_export_all).

-include_lib("society.hrl").

-behaviour(gen_server). %TODO: Develop as gen_agent

%% Defined agent species
-export([test_agent/0]).

%% Mutation functions
-export([test_mutation/1]).

%% gen_server callbacks
-export([init/1, handle_info/2, terminate/2]).

% hidden functions (for compatibility, do not modify)
-export([start_link/3]).
-export([handle_call/3, handle_cast/2, code_change/3]).

-define(AGENT, ?MODULE).

-record(state, {
    id :: agent_id(),
    governor :: pid(),
    properties :: #{}}).

%%%===================================================================
%%% Defined agents
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Definition of agents of same specie: different properties but same
%% mutation and call back functions. Normally placed here in different
%% calls which returns the properties.
%%
%% @end
%%--------------------------------------------------------------------
test_agent() ->
    _AgentProperties = #{
        % Extension of properties should be placed here
        score_base => rand:uniform(100)}.

%%%===================================================================
%%% Mutation functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% % TODO: To make description
%%
%% @end
%%--------------------------------------------------------------------
test_mutation(Properties) ->
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
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([Agent_Id, Governor, Properties]) ->
    process_flag(trap_exit, true), % Mandatory to catch supervisor exits
    timer:send_after(rand:uniform(10), score),
    timer:send_after(rand:uniform(10) * 100, terminate),
    {ok, #state{
        id         = Agent_Id,
        governor   = Governor,
        properties = Properties}}.

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
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).

handle_info(score, State) ->
    #state{id = Agent_Id, governor = Governor, properties = Properties} = State,
    #{score_base := ScoreBase} = Properties,
    eevo:add_score(Governor, Agent_Id, ScoreBase + rand:uniform(25)),
    {noreply, State};

handle_info(terminate, State) ->
    {stop, normal, State};

handle_info(_Info, State) ->
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
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
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
start_link(Agent_Id, Governor, Properties) -> gen_server:start_link(?MODULE, [Agent_Id, Governor, Properties], []).
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Request, State) -> {noreply, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.



