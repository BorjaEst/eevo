%%%-------------------------------------------------------------------
%%% @author Borja
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. oct 2018 11:40
%%%-------------------------------------------------------------------
-module(debugger_calls).
-author("Borja").

%% API
-export([eevo_SUITE/0, algorithms_SUITE/0]).

-define(LOG_DIR, "./apps/eevo/_build/test/logs").
-define(STEP_OPTS, []).
-define(DEFAULT_OPTIONS, [{logdir, ?LOG_DIR}, {step, ?STEP_OPTS}]).

%% TESTS CALLS
eevo_SUITE() ->
	ct:run_test([{suite, eevo_SUITE} | ?DEFAULT_OPTIONS]).

algorithms_SUITE() ->
	ct:run_test([{suite, algorithms_SUITE} | ?DEFAULT_OPTIONS]).




