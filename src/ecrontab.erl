-module(ecrontab).
-export([
    start/0,
    add/3, add/4,
    remove/1, remove/2,
    parse_spec/1
]).

-export([
    performance_test/1,
    loop_next_time/2, loop_next_time/3,
    loop_next_time_do/3
]).

%% ====================================================================
%% API
%% ====================================================================

start() ->
    application:ensure_all_started(?MODULE).

add(Name, Spec, MFA) ->
    add(Name, Spec, MFA, []).
add(Name, Spec0, {M,F,A}=MFA, Options) when is_atom(M) andalso is_atom(F) andalso is_atom(A) andalso is_list(Options) ->
    case parse_spec(Spec0) of
        {ok, Spec} ->
            ecrontab_task_manager:add(Name, Spec, MFA, Options);
        Err ->
            Err
    end.

remove(Name) ->
    remove(Name, []).
remove(Name, Options) ->
    ecrontab_task_manager:remove(Name, Options).

parse_spec(Spec) ->
    ecrontab_parse:parse_spec(Spec).

%% ====================================================================
%% for performance test
%% ====================================================================
performance_test(Count) ->
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