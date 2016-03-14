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
    R = ecrontab_sup:start_link(),
    ServerCount = min(8, erlang:system_info(schedulers_online)*2),
    [ecrontab_server_sup:start_child()|| _ <- lists:duplicate(ServerCount,ok)],
    R.

stop(_State) ->
    ok.


