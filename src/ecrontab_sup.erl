-module(ecrontab_sup).
-behaviour(supervisor).

-export([
    start_link/0,
    init/1
]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 2,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    ChildSpecs = [
        {ecrontab_worker_sup, {ecrontab_worker_sup, start_link, []},
            permanent, 2000, supervisor, [ecrontab_worker_sup]},
        {ecrontab_clock_tick, {ecrontab_clock_tick, start_link, []},
            permanent, 2000, worker, [ecrontab_clock_tick]}
    ],

    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
