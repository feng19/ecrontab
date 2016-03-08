-module(ecrontab_next_time_test).
-include_lib("eunit/include/eunit.hrl").
-include("ecrontab.hrl").

all_test_() ->
    NowDatetime = {{2016,3,7},{22,2,39}},
    NowTimestamp = 1457359359,
%%    ?debugFmt("NowDatetime:~p,NowTimestamp:~p~n",[NowDatetime,NowTimestamp]),
%%    List = interval_test_list(NowDatetime,NowTimestamp),
%%    ?debugFmt("List:~p",[List]),
    [
        every_second_test_list(NowDatetime,NowTimestamp),
        only_one_test_list(NowDatetime,NowTimestamp),
        interval_test_list(NowDatetime,NowTimestamp),
        timestamp_test_list(NowDatetime,NowTimestamp),
        normal_test_list(NowDatetime,NowTimestamp)
    ].

every_second_test_list(NowDatetime,NowTimestamp) ->
    {ok, Spec} = ecrontab_parse:parse_spec({'*','*','*','*','*','*','*'}),
    [
        ?_assertEqual({ok,1457359360},ecrontab_next_time:next_time(Spec,NowDatetime)),
        ?_assertEqual({ok,1457359360},ecrontab_next_time:next_time(Spec,NowTimestamp)),
        ?_assertEqual({ok,1457359360},ecrontab_next_time:next_time(Spec,NowDatetime,NowTimestamp)),
        ?_assertEqual([{{2016,3,7},{22,2,40}},{{2016,3,7},{22,2,41}},{{2016,3,7},{22,2,42}},{{2016,3,7},{22,2,43}},
            {{2016,3,7},{22,2,44}},{{2016,3,7},{22,2,45}},{{2016,3,7},{22,2,46}},{{2016,3,7},{22,2,47}},
            {{2016,3,7},{22,2,48}},{{2016,3,7},{22,2,49}}],next_time_loop_do(Spec,NowDatetime,NowTimestamp,10))
    ].

only_one_test_list(NowDatetime,NowTimestamp) ->
    {ok, Spec1} = ecrontab_parse:parse_spec({2016,'*','*','*','*','*','*'},[]),
    {ok, Spec2} = ecrontab_parse:parse_spec({'*',12,'*','*','*','*','*'}),
    {ok, Spec3} = ecrontab_parse:parse_spec({'*','*',6,'*','*','*','*'}),
    {ok, Spec4} = ecrontab_parse:parse_spec({'*','*','*',3,'*','*','*'}),
    {ok, Spec5} = ecrontab_parse:parse_spec({'*','*','*','*',23,'*','*'}),
    {ok, Spec6} = ecrontab_parse:parse_spec({'*','*','*','*','*',4,'*'}),
    {ok, Spec7} = ecrontab_parse:parse_spec({'*','*','*','*','*','*',55}),
    [
        % {{2016,3,7},{22,2,40}}
        ?_assertEqual({ok,1457359360}, ecrontab_next_time:next_time(Spec1,NowDatetime,NowTimestamp)),
        % {{2016,12,1},{0,0,0}}
        ?_assertEqual({ok,1480521600}, ecrontab_next_time:next_time(Spec2,NowDatetime,NowTimestamp)),
        % {{2016,4,6},{0,0,0}}
        ?_assertEqual({ok,1459872000}, ecrontab_next_time:next_time(Spec3,NowDatetime,NowTimestamp)),
        % {{2016,3,9},{0,0,0}}
        ?_assertEqual({ok,1457452800}, ecrontab_next_time:next_time(Spec4,NowDatetime,NowTimestamp)),
        % {{2016,3,7},{23,0,0}}
        ?_assertEqual({ok,1457362800}, ecrontab_next_time:next_time(Spec5,NowDatetime,NowTimestamp)),
        % {{2016,3,7},{22,4,0}}
        ?_assertEqual({ok,1457359440}, ecrontab_next_time:next_time(Spec6,NowDatetime,NowTimestamp)),
        % {{2016,3,7},{22,2,55}}
        ?_assertEqual({ok,1457359375}, ecrontab_next_time:next_time(Spec7,NowDatetime,NowTimestamp))
    ].

interval_test_list(NowDatetime,NowTimestamp) ->
    {ok, Spec1} = ecrontab_parse:parse_spec({<<"*/2">>,'*','*','*','*','*','*'}),
    [
        % {{2018,1,1},{0,0,0}}
        ?_assertEqual({ok,1514736000}, ecrontab_next_time:next_time(Spec1,NowDatetime,NowTimestamp)),
        ?_assertEqual([{{2018,1,1},{0,0,0}},{{2020,1,1},{0,0,0}},{{2022,1,1},{0,0,0}},{{2024,1,1},{0,0,0}},
            {{2026,1,1},{0,0,0}},{{2028,1,1},{0,0,0}},{{2030,1,1},{0,0,0}},{{2032,1,1},{0,0,0}},
            {{2034,1,1},{0,0,0}},{{2036,1,1},{0,0,0}}], next_time_loop_do(Spec1,NowDatetime,NowTimestamp,10))
    ].

timestamp_test_list(NowDatetime,NowTimestamp) ->
    %{2016, 2, 1, '*', 0, 0, 0}
    {ok, Spec} = ecrontab_parse:parse_spec({2016, 2, 1, '*', 0, 0, 0},[]),
    %{2016, 3, 7, '*', 22, 2, 40}
    {ok, SpecOK} = ecrontab_parse:parse_spec({2016, 3, 7, '*', 22, 2, 40},[]),
    [
        ?_assertEqual({false, time_over}, ecrontab_next_time:next_time(Spec,NowDatetime,NowTimestamp)),
        ?_assertEqual({ok, 1457359360}, ecrontab_next_time:next_time(SpecOK,NowDatetime,NowTimestamp))
    ].

normal_test_list(NowDatetime,NowTimestamp) ->
    %{'*', '*', '*', '*', '*', [5,15], 0}
    {ok, Spec1} = ecrontab_parse:parse_spec({'*', '*', '*', '*', '*', [5,15], 0}),
    [
        ?_assertEqual({ok, 1457359500}, ecrontab_next_time:next_time(Spec1,NowDatetime,NowTimestamp)),
        ?_assertEqual([{{2016,3,7},{22,5,0}},{{2016,3,7},{22,15,0}},{{2016,3,7},{23,5,0}},{{2016,3,7},{23,15,0}},
            {{2016,3,8},{0,5,0}},{{2016,3,8},{0,15,0}},{{2016,3,8},{1,5,0}},{{2016,3,8},{1,15,0}},
            {{2016,3,8},{2,5,0}},{{2016,3,8},{2,15,0}}],next_time_loop_do(Spec1,NowDatetime,NowTimestamp,10))
    ].


%% next_time_loop({'*', '*', '*', '*', '*', [5,15], 0},NowDatetime,NowTimestamp,20).
%% next_time_loop({'*', '*', '*', '*', '*', '*', 0},NowDatetime,NowTimestamp,20).
next_time_loop(Spec0,Count) ->
    NowDatetime = {{2016,3,7},{22,2,39}},
    NowTimestamp = 1457359359,
    next_time_loop(Spec0,NowDatetime,NowTimestamp,Count).
next_time_loop(Spec0,NowDatetime,NowTimestamp,Count) ->
    {ok, Spec} = ecrontab_parse:parse_spec(Spec0),
    next_time_loop_do(Spec,NowDatetime,NowTimestamp,Count).

next_time_loop_do(Spec,NowDatetime,NowTimestamp,1) ->
    {ok, Timestamp} = ecrontab_next_time:next_time(Spec,NowDatetime,NowTimestamp),
    Datetime = ecrontab_time_util:timestamp_to_datetime(Timestamp),
    [Datetime];
next_time_loop_do(Spec,NowDatetime,NowTimestamp,N) ->
    case ecrontab_next_time:next_time(Spec,NowDatetime,NowTimestamp) of
        {ok, Timestamp} ->
            Datetime = ecrontab_time_util:timestamp_to_datetime(Timestamp),
            [Datetime|next_time_loop_do(Spec,Datetime,Timestamp,N-1)];
        Err ->
            [Err]
    end.
