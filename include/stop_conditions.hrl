%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-define(stop_time(Time_ms),   fun(#{   runtime:=X})-> X>=Time_ms end).
-define(max_generations(N),   fun(#{generation:=X})-> X>=N       end).
-define(score_target(Target), fun(#{best_score:=X})-> X>=Target  end).

