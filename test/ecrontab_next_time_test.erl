-module(ecrontab_next_time_test).
-include("ecrontab.hrl").
-include_lib("eunit/include/eunit.hrl").

all_test_() ->
    NowDatetime = {{2016, 3, 7}, {22, 2, 39}},
    [
        every_second_test_list(NowDatetime),
        only_one_test_list(NowDatetime),
        interval_test_list(NowDatetime),
        timestamp_test_list(NowDatetime),
        normal_test_list(NowDatetime)
    ].

every_second_test_list(NowDatetime) ->
    {ok, Spec} = ecrontab_parse:parse_spec({'*', '*', '*', '*', '*', '*', '*'}),
    [
        ?_assertEqual({ok, ?DATETIME_TO_TIMESTAMP({{2016, 3, 7}, {22, 2, 40}})},
            ecrontab_next_time:next_time(Spec, NowDatetime, [{return, timestamp}])),
        ?_assertEqual([
            {{2016, 3, 7}, {22, 2, 40}}, {{2016, 3, 7}, {22, 2, 41}},
            {{2016, 3, 7}, {22, 2, 42}}, {{2016, 3, 7}, {22, 2, 43}},
            {{2016, 3, 7}, {22, 2, 44}}, {{2016, 3, 7}, {22, 2, 45}},
            {{2016, 3, 7}, {22, 2, 46}}, {{2016, 3, 7}, {22, 2, 47}},
            {{2016, 3, 7}, {22, 2, 48}}, {{2016, 3, 7}, {22, 2, 49}}
        ], next_time_loop_do(Spec, NowDatetime, 10))
    ].

only_one_test_list(NowDatetime) ->
    {ok, Spec1} = ecrontab_parse:parse_spec({2016, '*', '*', '*', '*', '*', '*'}, []),
    {ok, Spec2} = ecrontab_parse:parse_spec({'*', 12, '*', '*', '*', '*', '*'}),
    {ok, Spec3} = ecrontab_parse:parse_spec({'*', '*', 6, '*', '*', '*', '*'}),
    {ok, Spec4} = ecrontab_parse:parse_spec({'*', '*', '*', 3, '*', '*', '*'}),
    {ok, Spec5} = ecrontab_parse:parse_spec({'*', '*', '*', '*', 23, '*', '*'}),
    {ok, Spec6} = ecrontab_parse:parse_spec({'*', '*', '*', '*', '*', 4, '*'}),
    {ok, Spec7} = ecrontab_parse:parse_spec({'*', '*', '*', '*', '*', '*', 55}),
    [
        ?_assertEqual({ok, ?DATETIME_TO_TIMESTAMP({{2016, 3, 7}, {22, 2, 40}})},
            ecrontab_next_time:next_time(Spec1, NowDatetime, [{return, timestamp}])),
        ?_assertEqual([
            {{2016, 3, 7}, {22, 2, 40}}, {{2016, 3, 7}, {22, 2, 41}},
            {{2016, 3, 7}, {22, 2, 42}}, {{2016, 3, 7}, {22, 2, 43}},
            {{2016, 3, 7}, {22, 2, 44}}, {{2016, 3, 7}, {22, 2, 45}},
            {{2016, 3, 7}, {22, 2, 46}}, {{2016, 3, 7}, {22, 2, 47}},
            {{2016, 3, 7}, {22, 2, 48}}, {{2016, 3, 7}, {22, 2, 49}}
        ], next_time_loop_do(Spec1, NowDatetime, 10)),
        ?_assertEqual({ok, ?DATETIME_TO_TIMESTAMP({{2016, 12, 1}, {0, 0, 0}})},
            ecrontab_next_time:next_time(Spec2, NowDatetime, [{return, timestamp}])),
        ?_assertEqual([
            {{2016, 12, 1}, {0, 0, 0}}, {{2016, 12, 1}, {0, 0, 1}},
            {{2016, 12, 1}, {0, 0, 2}}, {{2016, 12, 1}, {0, 0, 3}},
            {{2016, 12, 1}, {0, 0, 4}}, {{2016, 12, 1}, {0, 0, 5}},
            {{2016, 12, 1}, {0, 0, 6}}, {{2016, 12, 1}, {0, 0, 7}},
            {{2016, 12, 1}, {0, 0, 8}}, {{2016, 12, 1}, {0, 0, 9}}
        ], next_time_loop_do(Spec2, NowDatetime, 10)),
        ?_assertEqual({ok, ?DATETIME_TO_TIMESTAMP({{2016, 4, 6}, {0, 0, 0}})},
            ecrontab_next_time:next_time(Spec3, NowDatetime, [{return, timestamp}])),
        ?_assertEqual([
            {{2016, 4, 6}, {0, 0, 0}}, {{2016, 4, 6}, {0, 0, 1}},
            {{2016, 4, 6}, {0, 0, 2}}, {{2016, 4, 6}, {0, 0, 3}},
            {{2016, 4, 6}, {0, 0, 4}}, {{2016, 4, 6}, {0, 0, 5}},
            {{2016, 4, 6}, {0, 0, 6}}, {{2016, 4, 6}, {0, 0, 7}},
            {{2016, 4, 6}, {0, 0, 8}}, {{2016, 4, 6}, {0, 0, 9}}
        ], next_time_loop_do(Spec3, NowDatetime, 10)),
        ?_assertEqual({ok, ?DATETIME_TO_TIMESTAMP({{2016, 3, 9}, {0, 0, 0}})},
            ecrontab_next_time:next_time(Spec4, NowDatetime, [{return, timestamp}])),
        ?_assertEqual([
            {{2016, 3, 9}, {0, 0, 0}}, {{2016, 3, 9}, {0, 0, 1}},
            {{2016, 3, 9}, {0, 0, 2}}, {{2016, 3, 9}, {0, 0, 3}},
            {{2016, 3, 9}, {0, 0, 4}}, {{2016, 3, 9}, {0, 0, 5}},
            {{2016, 3, 9}, {0, 0, 6}}, {{2016, 3, 9}, {0, 0, 7}},
            {{2016, 3, 9}, {0, 0, 8}}, {{2016, 3, 9}, {0, 0, 9}}
        ], next_time_loop_do(Spec4, NowDatetime, 10)),
        ?_assertEqual({ok, ?DATETIME_TO_TIMESTAMP({{2016, 3, 7}, {23, 0, 0}})},
            ecrontab_next_time:next_time(Spec5, NowDatetime, [{return, timestamp}])),
        ?_assertEqual([
            {{2016, 3, 7}, {23, 0, 0}}, {{2016, 3, 7}, {23, 0, 1}},
            {{2016, 3, 7}, {23, 0, 2}}, {{2016, 3, 7}, {23, 0, 3}},
            {{2016, 3, 7}, {23, 0, 4}}, {{2016, 3, 7}, {23, 0, 5}},
            {{2016, 3, 7}, {23, 0, 6}}, {{2016, 3, 7}, {23, 0, 7}},
            {{2016, 3, 7}, {23, 0, 8}}, {{2016, 3, 7}, {23, 0, 9}}
        ], next_time_loop_do(Spec5, NowDatetime, 10)),
        ?_assertEqual({ok, ?DATETIME_TO_TIMESTAMP({{2016, 3, 7}, {22, 4, 0}})},
            ecrontab_next_time:next_time(Spec6, NowDatetime, [{return, timestamp}])),
        ?_assertEqual([
            {{2016, 3, 7}, {22, 4, 0}}, {{2016, 3, 7}, {22, 4, 1}},
            {{2016, 3, 7}, {22, 4, 2}}, {{2016, 3, 7}, {22, 4, 3}},
            {{2016, 3, 7}, {22, 4, 4}}, {{2016, 3, 7}, {22, 4, 5}},
            {{2016, 3, 7}, {22, 4, 6}}, {{2016, 3, 7}, {22, 4, 7}},
            {{2016, 3, 7}, {22, 4, 8}}, {{2016, 3, 7}, {22, 4, 9}}
        ], next_time_loop_do(Spec6, NowDatetime, 10)),
        ?_assertEqual({ok, ?DATETIME_TO_TIMESTAMP({{2016, 3, 7}, {22, 2, 55}})},
            ecrontab_next_time:next_time(Spec7, NowDatetime, [{return, timestamp}])),
        ?_assertEqual([
            {{2016, 3, 7}, {22, 2, 55}}, {{2016, 3, 7}, {22, 3, 55}},
            {{2016, 3, 7}, {22, 4, 55}}, {{2016, 3, 7}, {22, 5, 55}},
            {{2016, 3, 7}, {22, 6, 55}}, {{2016, 3, 7}, {22, 7, 55}},
            {{2016, 3, 7}, {22, 8, 55}}, {{2016, 3, 7}, {22, 9, 55}},
            {{2016, 3, 7}, {22, 10, 55}}, {{2016, 3, 7}, {22, 11, 55}}
        ], next_time_loop_do(Spec7, NowDatetime, 10))
    ].

interval_test_list(NowDatetime) ->
    {ok, Spec1} = ecrontab_parse:parse_spec({<<"*/2">>, '*', '*', '*', '*', '*', '*'}),
    [
        ?_assertEqual({ok, ?DATETIME_TO_TIMESTAMP({{2018, 1, 1}, {0, 0, 0}})},
            ecrontab_next_time:next_time(Spec1, NowDatetime, [{return, timestamp}])),
        ?_assertEqual([
            {{2018, 1, 1}, {0, 0, 0}}, {{2020, 1, 1}, {0, 0, 0}},
            {{2022, 1, 1}, {0, 0, 0}}, {{2024, 1, 1}, {0, 0, 0}},
            {{2026, 1, 1}, {0, 0, 0}}, {{2028, 1, 1}, {0, 0, 0}},
            {{2030, 1, 1}, {0, 0, 0}}, {{2032, 1, 1}, {0, 0, 0}},
            {{2034, 1, 1}, {0, 0, 0}}, {{2036, 1, 1}, {0, 0, 0}}
        ], next_time_loop_do(Spec1, NowDatetime, 10))
    ].

timestamp_test_list(NowDatetime) ->
    {ok, Spec} = ecrontab_parse:parse_spec({2016, 2, 1, '*', 0, 0, 0}, []),
    {ok, SpecOK} = ecrontab_parse:parse_spec({2016, 3, 7, '*', 22, 2, 40}, []),
    [
        ?_assertEqual({false, time_over}, ecrontab_next_time:next_time(Spec, NowDatetime)),
        ?_assertEqual({ok, ?DATETIME_TO_TIMESTAMP({{2016, 3, 7}, {22, 2, 40}})},
            ecrontab_next_time:next_time(SpecOK, NowDatetime, [{return, timestamp}])),
        ?_assertEqual([{{2016, 3, 7}, {22, 2, 40}}, {false, time_over}], next_time_loop_do(SpecOK, NowDatetime, 10))
    ].

normal_test_list(NowDatetime) ->
    {ok, Spec1} = ecrontab_parse:parse_spec({'*', '*', '*', '*', '*', [5, 15], 0}),
    [
        ?_assertEqual({ok, ?DATETIME_TO_TIMESTAMP({{2016, 3, 7}, {22, 5, 0}})},
            ecrontab_next_time:next_time(Spec1, NowDatetime, [{return, timestamp}])),
        ?_assertEqual([
            {{2016, 3, 7}, {22, 5, 0}}, {{2016, 3, 7}, {22, 15, 0}},
            {{2016, 3, 7}, {23, 5, 0}}, {{2016, 3, 7}, {23, 15, 0}},
            {{2016, 3, 8}, {0, 5, 0}}, {{2016, 3, 8}, {0, 15, 0}},
            {{2016, 3, 8}, {1, 5, 0}}, {{2016, 3, 8}, {1, 15, 0}},
            {{2016, 3, 8}, {2, 5, 0}}, {{2016, 3, 8}, {2, 15, 0}}
        ], next_time_loop_do(Spec1, NowDatetime, 10))
    ].

%%next_time_loop(Spec0,Count) ->
%%    NowDatetime = {{2016,3,7},{22,2,39}},
%%    next_time_loop(Spec0,NowDatetime,Count).
%%next_time_loop(Spec0,NowDatetime,Count) ->
%%    {ok, Spec} = ecrontab_parse:parse_spec(Spec0, []),
%%    next_time_loop_do(Spec,NowDatetime,Count).

next_time_loop_do(Spec, NowDatetime, 1) ->
    {ok, Datetime} = ecrontab_next_time:next_time(Spec, NowDatetime),
    [Datetime];
next_time_loop_do(Spec, NowDatetime, N) ->
    case ecrontab_next_time:next_time(Spec, NowDatetime) of
        {ok, Datetime} ->
            [Datetime | next_time_loop_do(Spec, Datetime, N - 1)];
        Err ->
            [Err]
    end.
