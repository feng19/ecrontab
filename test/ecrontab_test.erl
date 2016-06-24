-module(ecrontab_test).
-include_lib("eunit/include/eunit.hrl").

all_test_() ->
    {setup,
        fun init/0,
        fun(_) -> ecrontab:stop() end,
        fun app_test_list/1
    }.

init() ->
    ecrontab:start(),
    ecrontab:add_worker(1).

app_test_list(_) ->
    Fun = fun() -> ok end,
    [
        ?_assertEqual(ok, ecrontab:add(1, {'*','*','*','*','*','*','*'}, Fun)),
        ?_assertEqual(ok, ecrontab:add(1, {'*','*','*','*','*','*','*'}, Fun)),
        ?_assertEqual(ok, ecrontab:add(1, {'*','*','*','*','*','*','*'}, Fun)),
        ?_assertEqual(ok, ecrontab:add(1, {'*','*','*','*','*','*','*'}, Fun)),
        ?_assertEqual(ok, ecrontab:add(1, {'*','*','*','*','*','*','*'}, Fun)),
        ?_assertEqual(ok, ecrontab:add(1, {'*','*','*','*','*','*','*'}, Fun)),
        ?_assertEqual(ok, ecrontab:add(1, {'*','*','*','*','*','*','*'}, Fun)),
        ?_assertEqual(ok, ecrontab:add(1, {'*','*','*','*','*','*','*'}, Fun))
    ].
