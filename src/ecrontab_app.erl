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
    AllConfig = application:get_all_env(),
    case ecrontab_sup:start_link() of
        {ok, Pid} ->
            start_more(AllConfig),
            {ok, Pid};
        Error ->
            Error
    end.

stop(_State) ->
    ok.

start_more(AllConfig) ->
    Workers = proplists:get_value(workers, AllConfig, []),
    ecrontab:init_workers(Workers).