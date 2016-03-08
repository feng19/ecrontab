-module(ecrontab).
-export([
    add/3, add/4,
    remove/1, remove/2,
    parse_spec/1
]).

-export([
    performance_test/1,
    loop_next_time/2, loop_next_time/4,
    loop_next_time_do/4
]).

%% ====================================================================
%% API
%% ====================================================================

add(Name, Spec, MFA) ->
    add(Name, Spec, MFA, []).
add(Name, Spec, MFA, Options) ->
    ecrontab_server:add(Name, Spec, MFA, Options).

remove(Name) ->
    remove(Name, []).
remove(Name, Options) ->
    ecrontab_server:remove(Name, Options).

parse_spec(Spec) ->
    ecrontab_parse:parse_spec(Spec).

%% ====================================================================
%% for performance test
%% ====================================================================
performance_test(Count) ->
    loop_next_time({'*', '*', '*', '*', '*', [5,15], 0},Count),
    loop_next_time({'*', '*', '*', '*', '*', '*', 0},Count).

loop_next_time(Spec,Count) ->
    NowDatetime = {{2016,3,7},{22,2,39}},
    NowTimestamp = 1457359359,
    loop_next_time(Spec,NowDatetime,NowTimestamp,Count).
loop_next_time(Spec0,NowDatetime,NowTimestamp,Count) ->
    {ok, Spec} = ecrontab_parse:parse_spec(Spec0),
    {Time,_} = timer:tc(?MODULE,loop_next_time_do,[Spec,NowDatetime,NowTimestamp,Count]),
    Ptime = Time/Count,
    Times = 1000000/Ptime,
    io:format("Spec0:~p,Count:~p,tc Time:~ps,per count time:~pus,one sec times:~p~n",[Spec0,Count,Time/1000000,Ptime,Times]).

loop_next_time_do(Spec,NowDatetime,NowTimestamp,1) ->
    ecrontab_next_time:next_time(Spec,NowDatetime,NowTimestamp),
    ok;
loop_next_time_do(Spec,NowDatetime,NowTimestamp,N) ->
    case ecrontab_next_time:next_time(Spec,NowDatetime,NowTimestamp) of
        {ok, Timestamp} ->
            Datetime = ecrontab_time_util:timestamp_to_datetime(Timestamp),
            loop_next_time_do(Spec,Datetime,Timestamp,N-1);
        Err ->
            Err
    end.