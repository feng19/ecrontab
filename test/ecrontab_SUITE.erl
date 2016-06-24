-module(ecrontab_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("ecrontab.hrl").
-compile([export_all]).

%% todo
all() ->
    [].

%%all() ->
%%    [
%%        {group, server},
%%        {group, next_sec_tasks},
%%%%        {group, check_servers},
%%        {group, add_tasks}
%%    ].
%%
%%groups() ->
%%    [
%%        {server, [sequence], [{group, add_worker},remove_server]},
%%        {add_worker, [parallel, {repeat, 2}], [add_worker]},
%%
%%        {check_servers, [], [{group, check_server}]},
%%        {check_server, [{repeat_until_any_fail, forever}], [check_server]},
%%
%%        {next_sec_tasks, [sequence, {repeat, 2}], [add_next_sec_task]},
%%
%%        {add_tasks, [parallel, {repeat, 10}], [add_task]}
%%    ].
%%
%%init_per_suite(Config) ->
%%    ecrontab:start(),
%%    Config.
%%
%%end_per_suite(_Config) ->
%%    ecrontab:stop(),
%%    ok.
%%
%%init_per_group(_, Config) ->
%%    Config.
%%
%%end_per_group(check_servers, Config) ->
%%    {Pid, Ref} = ?config(check_servers_pid, Config),
%%    Pid ! {stop, Ref},
%%    ok;
%%end_per_group(_,_Config) ->
%%    ok.
%%
%%init_per_testcase(check_server, Config) ->
%%    Tid = ets:first(ecrontab_check_servers_ct),
%%    [{check_server, Tid}|Config];
%%init_per_testcase(_,Config) ->
%%    Config.
%%
%%end_per_testcase(check_server, Config) ->
%%    Tid = ?config(check_server, Config),
%%    ets:delete(ecrontab_check_servers_ct, Tid),
%%    ok;
%%end_per_testcase(_, _Config) ->
%%    ok.
%%
%%add_worker(_Config) ->
%%    {ok, _} = ecrontab:add_worker().
%%
%%check_server(Config) ->
%%    Tid = ?config(check_server, Config),
%%    [Server] = ets:lookup(ecrontab_check_servers_ct, Tid),
%%%%    ct:pal("Server:~p~n",[Server]),
%%    TaskCount = Server#server.task_count,
%%    TaskCount = ets:info(Server#server.tid, size).
%%
%%remove_server(_Config) ->%todo
%%    todo.
%%
%%add_task(_Config) ->
%%    ok = ecrontab:add({'*','*','*','*','*','*','*'}, fun() -> ok end).
%%
%%add_next_sec_task(_Config) ->
%%    {{Y,M,D},{HH,MM,SS}} = ecrontab_time_util:next_second(erlang:localtime()),
%%    PID = self(),
%%    Ref = make_ref(),
%%    Fun = fun() -> PID ! {ok,Ref} end,
%%    ok = ecrontab:add({Y,M,D,'*',HH,MM,SS}, Fun),
%%    receive
%%        {ok, Ref} ->
%%            ok
%%    end.