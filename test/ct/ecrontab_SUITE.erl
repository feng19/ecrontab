-module(ecrontab_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("ecrontab.hrl").
-compile([export_all]).

all() ->
    [
        add_worker,
        add_worker_tasks,
        add_worker_tasks_not_auto_start,
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

init_per_testcase(TestCase, Config) when TestCase == add_task orelse
    TestCase == del_task orelse TestCase == add_next_sec_task ->
    WokerName = make_ref(),
    {ok, Pid} = ecrontab:add_worker(WokerName),
    [{worker, Pid} | Config];
init_per_testcase(_, Config) ->
    Config.

end_per_testcase(TestCase, Config) when TestCase == add_task andalso
    TestCase == del_task andalso TestCase == add_next_sec_task ->
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

add_worker_tasks(_) ->
    Ref = make_ref(),
    Self = self(),

    T1Fun = fun() -> Self ! {t1, Ref} end,
    {{Y1, M1, D1}, {HH1, MM1, SS1}} = erlang:localtime(),
    Spec1 = {Y1, M1, D1, '*', HH1, MM1, SS1},

    T2Fun = fun() -> Self ! {t2, Ref} end,
    {{Y2, M2, D2}, {HH2, MM2, SS2}} = ecrontab_time_util:next_second(ecrontab_time_util:next_second(erlang:localtime())),
    Spec2 = {Y2, M2, D2, '*', HH2, MM2, SS2},

    T3Fun = fun() -> Self ! {t3, Ref} end,
    Spec3 = {'*', '*', '*', '*', '*', '*', '*'},
    WorkerTasks = [
        {Spec1, T1Fun},
        {Spec2, T2Fun},
        {Spec3, T3Fun}
    ],
    {ok, _Pid} = ecrontab:add_worker(#{worker_name => ?FUNCTION_NAME, tasks => WorkerTasks}),
    receive
        {t1, Ref} ->
            error(test_error, ?FUNCTION_NAME)
    after 1000 -> ok
    end,
    receive
        {t2, Ref} -> ok
    after 2000 ->
        error(test_error, ?FUNCTION_NAME)
    end,
    receive
        {t3, Ref} -> ok
    after 1000 ->
        error(test_error, ?FUNCTION_NAME)
    end,
    ecrontab:stop_worker(?FUNCTION_NAME),
    ok.

add_worker_tasks_not_auto_start(_) ->
    Ref = make_ref(),
    Self = self(),
    Fun = fun() -> Self ! {t1, Ref} end,
    WorkerTasks = [
        {{'*', '*', '*', '*', '*', '*', '*'}, Fun}
    ],
    ecrontab:add_worker(#{worker_name => ?FUNCTION_NAME, is_auto_start => false, tasks => WorkerTasks}),
    receive
        {t1, Ref} ->
            error(test_error, ?FUNCTION_NAME)
    after 1000 -> ok
    end,
    ecrontab:start_worker_tasks(?FUNCTION_NAME),
    receive
        {t1, Ref} -> ok
    after 1000 ->
        error(test_error, ?FUNCTION_NAME)
    end,
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