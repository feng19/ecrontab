-module(ecrontab_test).
-include_lib("eunit/include/eunit.hrl").

all_test_() ->
    {setup,
        fun ecrontab:start/0,
        fun(_) -> ecrontab:stop() end,
        fun app_test_list/1
    }.

app_test_list(_) ->
    Fun = fun() -> ok end,
    [
        ?_assertEqual({ok,1},ecrontab:add(1,{'*','*','*','*','*','*','*'},Fun)),
        ?_assertEqual({ok,2},ecrontab:add(2,{'*','*','*','*','*','*','*'},Fun)),
        ?_assertEqual({ok,3},ecrontab:add(3,{'*','*','*','*','*','*','*'},Fun)),
        ?_assertEqual({ok,4},ecrontab:add(4,{'*','*','*','*','*','*','*'},Fun)),
        ?_assertEqual({ok,5},ecrontab:add(5,{'*','*','*','*','*','*','*'},Fun)),
        ?_assertEqual({ok,6},ecrontab:add(6,{'*','*','*','*','*','*','*'},Fun)),
        ?_assertEqual({ok,7},ecrontab:add(7,{'*','*','*','*','*','*','*'},Fun)),
        ?_assertEqual({ok,8},ecrontab:add(8,{'*','*','*','*','*','*','*'},Fun))
    ].
