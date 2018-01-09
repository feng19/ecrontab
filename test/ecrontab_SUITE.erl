-module(ecrontab_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("ecrontab.hrl").
-compile([export_all]).

all() ->
    [
        add_worker,
        {group, next_sec_tasks},
        {group, add_tasks},
        del_task
    ].

groups() ->
    [
        {next_sec_tasks, [sequence, {repeat, 2}], [add_next_sec_task]},

        {add_tasks, [parallel, {repeat, 10}], [add_task]}
    ].

init_per_suite(Config) ->
    ecrontab:start(),
    Config.

end_per_suite(_Config) ->
    ecrontab:stop(),
    ok.

init_per_group(_, Config) ->
    Config.

end_per_group(check_servers, Config) ->
    {Pid, Ref} = ?config(check_servers_pid, Config),
    Pid ! {stop, Ref},
    ok;
end_per_group(_, _Config) ->
    ok.

init_per_testcase(add_task, Config) ->
    WokerName = make_ref(),
    {ok, Pid} = ecrontab:add_worker(WokerName),
    [{worker, Pid} | Config];
init_per_testcase(del_task, Config) ->
    WokerName = make_ref(),
    {ok, Pid} = ecrontab:add_worker(WokerName),
    [{worker, Pid} | Config];
init_per_testcase(add_next_sec_task, Config) ->
    WokerName = make_ref(),
    {ok, Pid} = ecrontab:add_worker(WokerName),
    [{worker, Pid} | Config];
init_per_testcase(_, Config) ->
    Config.

end_per_testcase(add_task, Config) ->
    Pid = ?config(worker, Config),
    ok = ecrontab:stop_worker(Pid),
    ok;
end_per_testcase(del_task, Config) ->
    Pid = ?config(worker, Config),
    ok = ecrontab:stop_worker(Pid),
    ok;
end_per_testcase(add_next_sec_task, Config) ->
    Pid = ?config(worker, Config),
    ok = ecrontab:stop_worker(Pid),
    ok;
end_per_testcase(_, _Config) ->
    ok.

%% ====================================================================
%% test
%% ====================================================================

add_worker(_Config) ->
    {ok, Pid} = ecrontab:add_worker(?MODULE),
    {error, duplicate_name} = ecrontab:add_worker(?MODULE),
    true = erlang:is_process_alive(Pid),
    ok = ecrontab:stop_worker(?MODULE),
    {error, no_worker} = ecrontab:stop_worker(?MODULE),
    false = erlang:is_process_alive(Pid),
    ok.

add_task(Config) ->
    WorkerPid = ?config(worker, Config),
    ok = ecrontab:add(WorkerPid, {'*', '*', '*', '*', '*', '*', '*'}, fun() -> ok end).

del_task(Config) ->
    WorkerPid = ?config(worker, Config),
    P = self(),
    Ref = make_ref(),
    Fun = fun() -> P ! {ok, Ref} end,
    ok = ecrontab:add(WorkerPid, ?FUNCTION_NAME, {'*', '*', '*', '*', '*', '*', '*'}, Fun, []),
    ok = ecrontab:del(WorkerPid, ?FUNCTION_NAME),
    receive
        {ok, Ref} -> % It's ok first
            ok
    after 1000 ->
        ok
    end,
    receive
        {ok, Ref} -> % It's not ok second
            error(test_error, ?FUNCTION_NAME)
    after 1000 ->
        ok
    end.

add_next_sec_task(Config) ->
    WorkerPid = ?config(worker, Config),
    {{Y, M, D}, {HH, MM, SS}} = ecrontab_time_util:next_second(erlang:localtime()),
    P = self(),
    Ref = make_ref(),
    Fun = fun() -> P ! {ok, Ref} end,
    ok = ecrontab:add(WorkerPid, {Y, M, D, '*', HH, MM, SS}, Fun),
    receive
        {ok, Ref} ->
            ok
    after 1000 ->
        error(test_error, ?FUNCTION_NAME)
    end.