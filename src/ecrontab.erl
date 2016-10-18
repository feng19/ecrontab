-module(ecrontab).
-include("ecrontab.hrl").
-include("ecrontab_parse.hrl").
-export([
    start/0,
    stop/0,
    add/3, add/4,
    add_spec/4, add_spec/5,
    worker_list/0,
    add_worker/1,
    get_worker_count/0
]).

%% for test
-export([
    app_performance_test/3,
    parse_spec_performance_test/1,
    next_time_performance_test/1,
    loop_next_time/2, loop_next_time/3,
    loop_next_time_do/3
]).

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

add(Worker, Spec, MFA) ->
    add(Worker, Spec, MFA, []).
add(Worker, Spec, MFA, Options) when is_record(Spec, spec) ->
    add_spec(Worker, Spec, MFA, Options);
add(Worker, Spec0, MFA, Options) ->
    NowDatetime = erlang:localtime(),
    case ecrontab_parse:parse_spec(Spec0, [{filter_over_time,NowDatetime}]) of
        {ok, Spec} ->
            do_add(Worker, Spec, MFA, NowDatetime, Options);
        Err ->
            Err
    end.

add_spec(Worker, Spec, MFA, Options) ->
    add_spec(Worker, Spec, MFA, erlang:localtime(), Options).
add_spec(Worker, Spec, MFA, NowDatetime, Options) when is_record(Spec, spec) ->
    do_add(Worker, Spec, MFA, NowDatetime, Options).

worker_list() ->
    ets:tab2list(?ETS_WORKER_NAME_INDEX).

add_worker(Args) when is_list(Args) ->
    ecrontab_worker_sup:start_child(Args);
add_worker(WorkerName) ->
    ecrontab_worker_sup:start_child([WorkerName]).

get_worker_count() ->
    proplists:get_value(workers, supervisor:count_children(ecrontab_worker_sup)).

%% ====================================================================
%% internal API
%% ====================================================================

do_add(Worker, Spec, MFA, NowDatetime, Options) when is_list(Options) ->
    check_mfa(MFA),
    Task = #task{spec = Spec, mfa = MFA, add_time = NowDatetime, options = Options},
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

check_mfa(Fun) when is_function(Fun, 0) ->
    true;
check_mfa({M, F, A}=MFA) when is_atom(M) andalso is_atom(F) andalso is_list(A) ->
    case code:ensure_loaded(M) of
        {module, M} ->
            Arity = length(A),
            case erlang:function_exported(M, F, Arity) of
                true -> true;
                false ->
                    exit({error_mfa, MFA})
            end;
        Err ->
            exit({module_not_loaded, M, Err})
    end;
check_mfa({_Node,M,F,A}) when is_atom(M) andalso is_atom(F) andalso is_list(A) ->
    true;
check_mfa(MFA) ->
    exit({error_mfa, MFA}).

%% ====================================================================
%% app performance test
%% ====================================================================
app_performance_test(WorkerCount, Count, Secs) when Secs > 0 andalso Secs < 60 ->
    ecrontab:start(),
    eprof:start(),
    Self = self(),
    eprof:profile([Self]),
    [add_worker([N, Count]) || N <- lists:seq(1,WorkerCount)],
    Datetime = erlang:localtime(),
    SecList = app_performance_test_get_sec_list(5,Datetime,[]),
    {ok, Spec} = ecrontab_parse:parse_spec({'*','*','*','*','*','*',SecList},[]),
    Fun = fun() -> ok end,
    [begin
        [ok = ecrontab:add_spec(WorkerName,Spec,Fun,Datetime,[]) || _ <- lists:seq(1, Count)],
        ok
    end || {WorkerName, _WorkerPid} <- worker_list()],
    io:format("add spec ok"),
    timer:sleep(Secs*1000),
    ecrontab:stop(),
    eprof:stop_profiling(),
    eprof:analyze(),
    eprof:stop().

loop_wait(0) -> ok;
loop_wait(Count) ->
    receive
        ok ->
            loop_wait(Count-1)
    end.

app_performance_test_get_sec_list(0,_,List) ->
    List;
app_performance_test_get_sec_list(Secs,NowDatetime,List) ->
    Datetime = ecrontab_time_util:next_second(NowDatetime),
    Sec = ecrontab_time_util:get_datetime_second(Datetime),
    app_performance_test_get_sec_list(Secs-1,Datetime,[Sec|List]).

%% ====================================================================
%% parse_spec performance test
%% ====================================================================

parse_spec_performance_test(Count) ->
    Tests =
    [
        {'*','*','*','*','*','*','*'},
        {3016,'*','*','*','*','*','*'},
        {'*',[12],'*','*','*','*','*'},
        {'*','*',6,'*','*','*','*'},
        {'*','*','*',3,'*','*','*'},
        {'*','*','*','*',23,'*','*'},
        {'*','*','*','*','*',4,'*'},
        {'*','*','*','*','*','*',55},
        {<<"3016-3018">>,'*','*','*','*','*','*'},
        {<<"3016-3024/2">>,'*','*','*','*','*','*'},
        {<<"3016-3028/5">>,'*','*','*','*','*','*'},
        {[3016,3017,3018,2010],'*','*','*','*','*','*'},
        {2016, 4, '*', 3, 4, 5, 0},
        {'*','*','*','*','*','*',[6,7,8,10]}
    ],
    Self = self(),
    eprof:start(),
    eprof:profile([Self]),
    [spawn(fun() ->
        loop_parse_spec(Spec,Count),
        Self ! ok
    end) || Spec<-Tests],
    loop_wait(length(Tests)),
    eprof:stop_profiling(),
    eprof:analyze(),
    eprof:stop().

loop_parse_spec(_Spec, 0) ->
    ok;
loop_parse_spec(Spec, Count) ->
    ecrontab_parse:parse_spec(Spec, []),
    loop_parse_spec(Spec, Count-1).


%% ====================================================================
%% next_time performance test
%% ====================================================================

next_time_performance_test(Count) ->
    Tests =[
        {'*', '*', '*', '*', '*', [5,15], 0},
        {'*', '*', '*', '*', '*', '*', 0}
    ],
    Self = self(),
    eprof:start(),
    eprof:profile([Self]),
    [spawn(fun() ->
        loop_next_time(Spec,Count),
        Self ! ok
    end) || Spec<-Tests],
    loop_wait(length(Tests)),
    eprof:stop_profiling(),
    eprof:analyze(),
    eprof:stop().

loop_next_time(Spec,Count) ->
    NowDatetime = {{2016,3,7},{22,2,39}},
    loop_next_time(Spec,NowDatetime,Count).
loop_next_time(Spec0,NowDatetime,Count) ->
    {ok, Spec} = ecrontab_parse:parse_spec(Spec0, []),
    {Time,_} = timer:tc(?MODULE,loop_next_time_do,[Spec,NowDatetime,Count]),
    Ptime = Time/Count,
    Times = 1000000/Ptime,
    io:format("Spec0:~p,Count:~p,tc Time:~ps,per count time:~pus,one sec times:~p~n",[Spec0,Count,Time/1000000,Ptime,Times]).

loop_next_time_do(Spec,NowDatetime,1) ->
    ecrontab_next_time:next_time(Spec,NowDatetime),
    ok;
loop_next_time_do(Spec,NowDatetime,N) ->
    case ecrontab_next_time:next_time(Spec,NowDatetime) of
        {ok, Datetime} ->
            loop_next_time_do(Spec,Datetime,N-1);
        Err ->
            Err
    end.
