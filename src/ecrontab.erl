-module(ecrontab).
-export([
    start/0,
    stop/0,
    add/3, add/4,
    remove/1, remove/2
]).

%% for test
-export([
    app_performance_test/2,

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

add(Name, Spec, MFA) ->
    add(Name, Spec, MFA, []).
add(Name, Spec0, {M,F,A}=MFA, Options) when is_atom(M) andalso is_atom(F) andalso is_list(A) ->
    do_add(Name, Spec0, MFA, Options);
add(Name, Spec0, Fun, Options) when is_function(Fun, 0) ->
    do_add(Name, Spec0, Fun, Options).

do_add(Name, Spec0, MFA, Options) when is_list(Options) ->
    NowDatetime = erlang:localtime(),
    case ecrontab_parse:parse_spec(Spec0, [{filter_over_time,NowDatetime}]) of
        {ok, Spec} ->
            ecrontab_task_manager:add(Name, Spec, MFA, NowDatetime, Options);
        Err ->
            Err
    end.

remove(Name) ->
    remove(Name, []).
remove(Name, Options) ->
    ecrontab_task_manager:remove(Name, Options).

%% ====================================================================
%% app performance test
%% ====================================================================
app_performance_test(Count,Secs) when Secs > 0 andalso Secs < 60 ->
    ecrontab:start(),
    eprof:start(),
    Self = self(),
    eprof:profile([Self]),
    if
        Count > 80000 ->
            AddChildCount0 = (Count - 80000) / 10000,
            AddChildCount1 = erlang:trunc(AddChildCount0),
            AddChildCount =
            case AddChildCount0 - AddChildCount1 == 0 of
                true ->
                    AddChildCount1;
                _ ->
                    AddChildCount1+1
            end,
            [ecrontab_server_sup:start_child()||_ <- lists:seq(1,AddChildCount)];
        true ->
            none
    end,
    SecList = app_performance_test_get_sec_list(Secs,erlang:localtime(),[]),
    [{ok, Name} = ecrontab:add(Name,{'*','*','*','*','*','*',SecList},fun() -> ok end) ||
        Name <- lists:seq(1,Count)],
    timer:sleep(Secs*1000),
    ecrontab:stop(),
    eprof:stop_profiling(),
    eprof:analyze(),
    eprof:stop().

app_performance_test_get_sec_list(0,_,List) ->
    List;
app_performance_test_get_sec_list(Secs,NowDatetime,List) ->
    Datetime = ecrontab_time_util:next_second(NowDatetime),
    Sec = ecrontab_time_util:get_datetime_second(Datetime),
    app_performance_test_get_sec_list(Secs-1,Datetime,[Sec|List]).

%% ====================================================================
%% next_time performance test
%% ====================================================================

next_time_performance_test(Count) ->
    eprof:start(),
    eprof:profile([self()]),
    Tests =[
        {'*', '*', '*', '*', '*', [5,15], 0},
        {'*', '*', '*', '*', '*', '*', 0}
    ],
    [loop_next_time(Spec,Count) || Spec<-Tests],
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