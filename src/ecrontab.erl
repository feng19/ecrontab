-module(ecrontab).
-include("ecrontab.hrl").
-include("ecrontab_parse.hrl").
-export([
    start/0,
    stop/0,
    add/3, add/4, add/5,
    add_spec/4, add_spec/5, add_spec/6,
    del/2,
    worker_list/0,
    add_workers/1,
    add_worker/1,
    stop_worker/1,
    start_worker_tasks/1,
    get_worker_count/0,
    parse_tasks/1,
    parse_task/1
]).

%% for test
-export([
    app_performance_test/3,
    parse_spec_performance_test/1,
    next_time_performance_test/1,
    loop_next_time/2, loop_next_time/3,
    loop_next_time_do/3
]).

-export_type([
    spec_type/0, spec_field_type/0, spec_field_value/0, spec_field/0, spec/0,
    ecrontab_mfa/0, add_result/0, worker_name/0,
    worker_setting/0, worker_settings/0,
    worker_tasks/0, worker_task/0,
    task/0, task_name/0
]).

-type spec_type() ::
    ?SPEC_TYPE_NORMAL
    | ?SPEC_TYPE_TIMESTAMP
    | ?SPEC_TYPE_EVERY_SECOND
    | ?SPEC_TYPE_INTERVAL_YEAR
    | ?SPEC_TYPE_ONLY_ONE.

-type spec_field_type() ::
    ?SPEC_FIELD_TYPE_ANY
    | ?SPEC_FIELD_TYPE_NUM
    | ?SPEC_FIELD_TYPE_LIST
    | ?SPEC_FIELD_TYPE_INTERVAL.

-type spec_field_any() :: ?SPEC_FIELD_ANY.
-type spec_field_num() :: non_neg_integer().
-type spec_field_list() :: [spec_field_num()].
-type spec_field_interval() :: non_neg_integer().

-type spec_field_value() ::
    spec_field_any() |
    spec_field_num() |
    spec_field_list() |
    spec_field_interval().

-type spec_field() :: #spec_field{}.

-type spec() :: #spec{}.

-type ecrontab_mfa() :: function() | mfa() | {node(), module(), atom(), list()}.
-type add_result() :: ok | {error, Reason :: any()}.
-type worker_name() :: any().

-type worker_settings() :: [worker_setting()].
-type worker_setting() ::
    {worker_name()} |
    {worker_name(), max_task_count()} |
    {worker_name(), max_task_count(), worker_tasks()} |
    {worker_name(), is_worker_auto_start()} |
    {worker_name(), is_worker_auto_start(), worker_tasks()} |
    {worker_name(), max_task_count(), is_worker_auto_start(), worker_tasks()} |
    #{
        worker_name := worker_name(), max_task_count => max_task_count(),
        is_worker_auto_start => is_worker_auto_start(), worker_tasks => worker_tasks()
    }.

-type max_task_count() :: pos_integer().
-type is_worker_auto_start() :: boolean().
-type worker_tasks() :: [worker_task()].
-type worker_task() ::
    task() |
    {spec() | ecrontab_parse:parse_spec(), ecrontab_mfa()} |
    {task_name(), spec() | ecrontab_parse:parse_spec(), ecrontab_mfa()} |
    {task_name(), spec() | ecrontab_parse:parse_spec(), ecrontab_mfa(), task_options()}.

-type task() :: #task{}.
-type task_name() :: undefined | any().
-type task_options() :: proplists:proplist().

%% ====================================================================
%% app
%% ====================================================================

start() ->
    application:ensure_all_started(?MODULE).

stop() ->
    application:stop(?MODULE).

%% ====================================================================
%% API
%% ====================================================================

-spec add(worker_name(), ecrontab_parse:parse_spec() | spec(), ecrontab_mfa()) ->
    add_result().
add(WorkerName, Spec, MFA) ->
    add(WorkerName, Spec, MFA, []).
-spec add(worker_name(), Spec :: ecrontab_parse:parse_spec() | spec(), ecrontab_mfa(), task_options()) ->
    add_result().
add(WorkerName, Spec, MFA, Options) when is_record(Spec, spec) ->
    add_spec(WorkerName, Spec, MFA, Options);
add(WorkerName, ParseSpec, MFA, Options) ->
    add(WorkerName, undefined, ParseSpec, MFA, Options).
-spec add(worker_name(), task_name(), ecrontab_parse:parse_spec() | spec(),
    ecrontab_mfa(), task_options()) -> add_result().
add(WorkerName, Name, Spec0, MFA, Options) ->
    NowDatetime = erlang:localtime(),
    case ecrontab_parse:parse_spec(Spec0, [{filter_over_time, NowDatetime}]) of
        {ok, Spec} ->
            do_add(WorkerName, Name, Spec, MFA, NowDatetime, Options);
        Error ->
            Error
    end.

-spec add_spec(worker_name(), spec(), ecrontab_mfa(), task_options()) ->
    add_result().
add_spec(WorkerName, Spec, MFA, Options) ->
    add_spec(WorkerName, Spec, MFA, erlang:localtime(), Options).
-spec add_spec(worker_name(), spec(), ecrontab_mfa(), calendar:datetime(), task_options()) ->
    add_result().
add_spec(WorkerName, Spec, MFA, NowDatetime, Options) when is_record(Spec, spec) ->
    do_add(WorkerName, undefined, Spec, MFA, NowDatetime, Options).
-spec add_spec(worker_name(), task_name(), spec(), ecrontab_mfa(),
    calendar:datetime(), task_options()) -> add_result().
add_spec(WorkerName, Name, Spec, MFA, NowDatetime, Options) when is_record(Spec, spec) ->
    do_add(WorkerName, Name, Spec, MFA, NowDatetime, Options).

-spec del(worker_name(), task_name()) -> ok | {error, Reason :: any()}.
del(WorkerPid, Name) when is_pid(WorkerPid) ->
    ecrontab_worker:del(WorkerPid, Name);
del(WorkerName, TaskName) ->
    case ets:lookup(?ETS_WORKER_NAME_INDEX, WorkerName) of
        [{_, WorkerPid}] ->
            ecrontab_worker:del(WorkerPid, TaskName);
        _ ->
            {error, no_worker}
    end.

-spec worker_list() ->
    [{worker_name(), Pid :: pid()}, ...].
worker_list() ->
    ets:tab2list(?ETS_WORKER_NAME_INDEX).

-spec add_workers(worker_settings()) -> ok.
add_workers([]) -> ok;
add_workers([WorkerSettings | Workers]) ->
    case add_worker(WorkerSettings) of
        {ok, _} ->
            add_workers(Workers);
        Error -> Error
    end.

-spec add_worker(worker_name() | worker_setting()) -> supervisor:startchild_ret().
add_worker(Args) when is_list(Args) ->
    ecrontab_worker_sup:start_child(Args);
add_worker(WorkerSetting) when is_tuple(WorkerSetting) ->
    ecrontab_worker_sup:start_child(WorkerSetting);
add_worker(WorkerSetting) when is_map(WorkerSetting) ->
    ecrontab_worker_sup:start_child(WorkerSetting);
add_worker(WorkerName) ->
    ecrontab_worker_sup:start_child({WorkerName}).

-spec stop_worker(worker_name()) -> ok | {error, no_worker} | {error, atom()}.
stop_worker(WorkerPid) when is_pid(WorkerPid) ->
    supervisor:terminate_child(ecrontab_worker_sup, WorkerPid);
stop_worker(WorkerName) ->
    case ets:lookup(?ETS_WORKER_NAME_INDEX, WorkerName) of
        [{_, WorkerPid}] ->
            supervisor:terminate_child(ecrontab_worker_sup, WorkerPid);
        _ ->
            {error, no_worker}
    end.

-spec start_worker_tasks(worker_name()) -> ok | {error, no_worker}.
start_worker_tasks(WorkerName) ->
    case ets:lookup(?ETS_WORKER_NAME_INDEX, WorkerName) of
        [{_, WorkerPid}] ->
            ecrontab_worker:start_tasks(WorkerPid);
        _ ->
            {error, no_worker}
    end.

-spec get_worker_count() -> Count :: integer().
get_worker_count() ->
    proplists:get_value(workers, supervisor:count_children(ecrontab_worker_sup)).

-spec parse_tasks(worker_tasks()) -> {ok, [#task{}]} | {error, any()}.
parse_tasks(WorkerTasks) ->
    parse_tasks(WorkerTasks, []).
parse_tasks([], WorkerTasks) -> {ok, WorkerTasks};
parse_tasks([WorkerTask | WorkerTasks], Acc) ->
    case parse_task(WorkerTask) of
        {ok, Task} ->
            parse_tasks(WorkerTasks, [Task | Acc]);
        {error, Reason} ->
            {error, {WorkerTask, Reason}}
    end.

-spec parse_task(worker_task()) -> {ok, #task{}} | {error, any()}.
parse_task({Spec0, MFA}) ->
    parse_task({undefined, Spec0, MFA});
parse_task({Name, Spec0, MFA}) ->
    parse_task({Name, Spec0, MFA, []});
parse_task({Name, Spec0, MFA, Options}) ->
    check_mfa(MFA),
    NowDatetime = erlang:localtime(),
    case parse_task_parse_spec(Spec0, NowDatetime) of
        {ok, Spec} ->
            {ok, #task{name = Name, spec = Spec, mfa = MFA, add_time = NowDatetime, options = Options}};
        Error -> Error
    end.

%% ====================================================================
%% internal API
%% ====================================================================

do_add(Worker, Name, Spec, MFA, NowDatetime, Options) when is_list(Options) ->
    check_mfa(MFA),
    Task = #task{name = Name, spec = Spec, mfa = MFA, add_time = NowDatetime, options = Options},
    do_add(Worker, Task).

do_add(WorkerPid, Task) when is_pid(WorkerPid) ->
    ecrontab_worker:add(WorkerPid, Task);
do_add(WorkerName, Task) ->
    case ets:lookup(?ETS_WORKER_NAME_INDEX, WorkerName) of
        [{_, WorkerPid}] ->
            ecrontab_worker:add(WorkerPid, Task);
        _ ->
            {error, no_worker}
    end.

-spec check_mfa(ecrontab_mfa()) -> true.
check_mfa(Fun) when is_function(Fun, 0) ->
    true;
check_mfa({M, F, A} = MFA) when is_atom(M) andalso is_atom(F) andalso is_list(A) ->
    case code:ensure_loaded(M) of
        {module, M} ->
            Arity = length(A),
            case erlang:function_exported(M, F, Arity) of
                true -> true;
                false ->
                    exit({error_mfa, MFA})
            end;
        Error ->
            exit({module_not_loaded, M, Error})
    end;
check_mfa({_Node, M, F, A}) when is_atom(M) andalso is_atom(F) andalso is_list(A) ->
    true;
check_mfa(MFA) ->
    exit({error_mfa, MFA}).

parse_task_parse_spec(Spec, _NowDatetime) when is_record(Spec, spec) -> Spec;
parse_task_parse_spec(Spec0, NowDatetime) ->
    ecrontab_parse:parse_spec(Spec0, [{filter_over_time, NowDatetime}]).

%% ====================================================================
%% app performance test
%% ====================================================================
app_performance_test(WorkerCount, Count, Secs) when Secs > 0 andalso Secs < 60 ->
    start(),
    eprof:start(),
    Self = self(),
    eprof:profile([Self]),
    [add_worker([N, Count]) || N <- lists:seq(1, WorkerCount)],
    Datetime = erlang:localtime(),
    SecList = app_performance_test_get_sec_list(5, Datetime, []),
    {ok, Spec} = ecrontab_parse:parse_spec({'*', '*', '*', '*', '*', '*', SecList}, []),
    Fun = fun() -> ok end,
    [begin
         [ok = add_spec(WorkerName, Spec, Fun, Datetime, []) || _ <- lists:seq(1, Count)],
         ok
     end || {WorkerName, _WorkerPid} <- worker_list()],
    io:format("add spec ok"),
    timer:sleep(Secs * 1000),
    stop(),
    eprof:stop_profiling(),
    eprof:analyze(),
    eprof:stop().

loop_wait(0) -> ok;
loop_wait(Count) ->
    receive
        ok ->
            loop_wait(Count - 1)
    end.

app_performance_test_get_sec_list(0, _, List) ->
    List;
app_performance_test_get_sec_list(Secs, NowDatetime, List) ->
    Datetime = ecrontab_time_util:next_second(NowDatetime),
    Sec = ecrontab_time_util:get_datetime_second(Datetime),
    app_performance_test_get_sec_list(Secs - 1, Datetime, [Sec | List]).

%% ====================================================================
%% parse_spec performance test
%% ====================================================================

parse_spec_performance_test(Count) ->
    Tests =
        [
            {'*', '*', '*', '*', '*', '*', '*'},
            {3016, '*', '*', '*', '*', '*', '*'},
            {'*', [12], '*', '*', '*', '*', '*'},
            {'*', '*', 6, '*', '*', '*', '*'},
            {'*', '*', '*', 3, '*', '*', '*'},
            {'*', '*', '*', '*', 23, '*', '*'},
            {'*', '*', '*', '*', '*', 4, '*'},
            {'*', '*', '*', '*', '*', '*', 55},
            {<<"3016-3018">>, '*', '*', '*', '*', '*', '*'},
            {<<"3016-3024/2">>, '*', '*', '*', '*', '*', '*'},
            {<<"3016-3028/5">>, '*', '*', '*', '*', '*', '*'},
            {[3016, 3017, 3018, 2010], '*', '*', '*', '*', '*', '*'},
            {2016, 4, '*', 3, 4, 5, 0},
            {'*', '*', '*', '*', '*', '*', [6, 7, 8, 10]}
        ],
    Self = self(),
    eprof:start(),
    eprof:profile([Self]),
    [spawn(fun() ->
        loop_parse_spec(Spec, Count),
        Self ! ok
           end) || Spec <- Tests],
    loop_wait(length(Tests)),
    eprof:stop_profiling(),
    eprof:analyze(),
    eprof:stop().

loop_parse_spec(_Spec, 0) ->
    ok;
loop_parse_spec(Spec, Count) ->
    ecrontab_parse:parse_spec(Spec, []),
    loop_parse_spec(Spec, Count - 1).

%% ====================================================================
%% next_time performance test
%% ====================================================================

next_time_performance_test(Count) ->
    Tests = [
        {'*', '*', '*', '*', '*', [5, 15], 0},
        {'*', '*', '*', '*', '*', '*', 0}
    ],
    Self = self(),
    eprof:start(),
    eprof:profile([Self]),
    [spawn(fun() ->
        loop_next_time(Spec, Count),
        Self ! ok
           end) || Spec <- Tests],
    loop_wait(length(Tests)),
    eprof:stop_profiling(),
    eprof:analyze(),
    eprof:stop().

loop_next_time(Spec, Count) ->
    NowDatetime = {{2016, 3, 7}, {22, 2, 39}},
    loop_next_time(Spec, NowDatetime, Count).
loop_next_time(Spec0, NowDatetime, Count) ->
    {ok, Spec} = ecrontab_parse:parse_spec(Spec0, []),
    {Time, _} = timer:tc(?MODULE, loop_next_time_do, [Spec, NowDatetime, Count]),
    Ptime = Time / Count,
    Times = 1000000 / Ptime,
    io:format("Spec0:~p,Count:~p,tc Time:~ps,per count time:~pus,one sec times:~p~n", [Spec0, Count, Time / 1000000, Ptime, Times]).

loop_next_time_do(Spec, NowDatetime, 1) ->
    ecrontab_next_time:next_time(Spec, NowDatetime),
    ok;
loop_next_time_do(Spec, NowDatetime, N) ->
    case ecrontab_next_time:next_time(Spec, NowDatetime) of
        {ok, Datetime} ->
            loop_next_time_do(Spec, Datetime, N - 1);
        Error ->
            Error
    end.
