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
    ecrontab_server:init_servers(),
    R.

stop(_State) ->
    ok.


