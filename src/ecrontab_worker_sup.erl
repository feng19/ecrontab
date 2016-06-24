-module(ecrontab_worker_sup).
-behaviour(supervisor).

-export([
    start_link/0,
    start_child/1,
    init/1
]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Args) ->
    supervisor:start_child(?SERVER, [Args]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    AChild = {ecrontab_worker, {ecrontab_worker, start_link, []},
        transient, 2000, worker, [ecrontab_worker]},

    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
