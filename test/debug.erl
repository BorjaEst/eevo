%%%-------------------------------------------------------------------
%%% @author Borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(debug).
-author("Borja").

%% API
-export([eevo_SUITE/0]).

-define(LOG_DIR, "./apps/eevo/_build/test/logs").
-define(STEP_OPTS, []).
-define(DEFAULT_OPTIONS, [{logdir, ?LOG_DIR}, {step, ?STEP_OPTS}]).

%% TESTS CALLS
eevo_SUITE() ->
    ct:run_test([{suite, eevo_SUITE} | ?DEFAULT_OPTIONS]).




