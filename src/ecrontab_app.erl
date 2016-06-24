-module(ecrontab_app).
-behaviour(application).
-include("ecrontab.hrl").

-export([
    start/2,
    stop/1
]).


start(_StartType, _StartArgs) ->
    %todo mnesia sava tasks
    pg2:create(?GROUP_NAME),
    ets:new(?ETS_WORKER_NAME_INDEX, [public,named_table,{keypos,1},{write_concurrency, true},{read_concurrency, true}]),
    ecrontab_sup:start_link().

stop(_State) ->
    ok.


