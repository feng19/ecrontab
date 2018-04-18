%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ecrontab_time_util_tests).

-include_lib("eunit/include/eunit.hrl").


validate_week_test_() ->
    WeeksTests = [
        {{ok, 1}, [<<"Mon">>, <<"mon">>, <<"Monday">>, <<"monday">>]},
        {{ok, 2}, [<<"Tue">>, <<"tue">>, <<"Tuesday">>, <<"tuesday">>]},
        {{ok, 3}, [<<"Wed">>, <<"wed">>, <<"Wednesday">>, <<"wednesday">>]},
        {{ok, 4}, [<<"Thu">>, <<"thu">>, <<"Thursday">>, <<"thursday">>]},
        {{ok, 5}, [<<"Fri">>, <<"fri">>, <<"Friday">>, <<"friday">>]},
        {{ok, 6}, [<<"Sat">>, <<"sat">>, <<"Saturday">>, <<"saturday">>]},
        {{ok, 7}, [<<"Sun">>, <<"sun">>, <<"Sunday">>, <<"sunday">>]}
    ],

    [
        ?_assertEqual({ok, Week}, ecrontab_time_util:validate_week(Week)) || Week <- lists:seq(1, 7)
    ] ++ [
        [?_assertEqual(Expect, ecrontab_time_util:validate_week(Week)) || Week <- Weeks]
        || {Expect, Weeks} <- WeeksTests
    ] ++ [
        [?_assertEqual(Expect, ecrontab_time_util:validate_week(binary_to_list(Week))) || Week <- Weeks]
        || {Expect, Weeks} <- WeeksTests
    ] ++ [
        [?_assertEqual(Expect, ecrontab_time_util:validate_week(binary_to_atom(Week, utf8))) || Week <- Weeks]
        || {Expect, Weeks} <- WeeksTests
    ] ++ [

        ?_assertEqual({ok, 7}, ecrontab_time_util:validate_week(0)),
        ?_assertEqual({ok, 7}, ecrontab_time_util:validate_week(<<"0">>)),
        ?_assertEqual({error, invalid_value}, ecrontab_time_util:validate_week(-1)),
        ?_assertEqual({error, invalid_value}, ecrontab_time_util:validate_week(8))
    ].



validate_month_test_() ->
    MonthsTests = [
        {{ok, 1}, [<<"January">>, <<"january">>, <<"Jan">>, <<"jan">>]},
        {{ok, 2}, [<<"February">>, <<"february">>, <<"Feb">>, <<"feb">>]},
        {{ok, 3}, [<<"March">>, <<"march">>, <<"Mar">>, <<"mar">>]},
        {{ok, 4}, [<<"April">>, <<"april">>, <<"Apr">>, <<"apr">>]},
        {{ok, 5}, [<<"May">>, <<"may">>]},
        {{ok, 6}, [<<"Jun">>, <<"jun">>, <<"June">>, <<"june">>]},
        {{ok, 7}, [<<"Jul">>, <<"jul">>, <<"July">>, <<"july">>]},
        {{ok, 8}, [<<"Aug">>, <<"aug">>, <<"August">>, <<"august">>]},
        {{ok, 9}, [<<"Sep">>, <<"sep">>, <<"September">>, <<"september">>]},
        {{ok, 10}, [<<"October">>, <<"october">>, <<"Oct">>, <<"oct">>]},
        {{ok, 11}, [<<"November">>, <<"november">>, <<"Nov">>, <<"nov">>]},
        {{ok, 12}, [<<"December">>, <<"december">>, <<"Dec">>, <<"dec">>]}
    ],
    [
        ?_assertEqual({ok, Month}, ecrontab_time_util:validate_month(Month)) || Month <- lists:seq(1, 12)
    ] ++ [
        [?_assertEqual(Expect, ecrontab_time_util:validate_month(Month)) || Month <- Months]
        || {Expect, Months} <- MonthsTests
    ] ++ [
        [?_assertEqual(Expect, ecrontab_time_util:validate_month(binary_to_list(Month))) || Month <- Months]
        || {Expect, Months} <- MonthsTests
    ] ++ [
        [?_assertEqual(Expect, ecrontab_time_util:validate_month(binary_to_atom(Month, utf8))) || Month <- Months]
        || {Expect, Months} <- MonthsTests
    ] ++ [
        ?_assertEqual({error, invalid_value}, ecrontab_time_util:validate_month(-1)),
        ?_assertEqual({error, invalid_value}, ecrontab_time_util:validate_month(13))
    ].

next_year_test_() ->
    [
        ?_assertEqual({{2001, 1, 1}, {0, 0, 0}}, ecrontab_time_util:next_year({{2000, 1, 1}, {0, 0, 0}})),
        ?_assertEqual({{2001, 3, 1}, {0, 0, 0}}, ecrontab_time_util:next_year({{2000, 2, 29}, {0, 0, 0}}))
    ].

next_month_test_() ->
    [
        ?_assertEqual({{2000, 2, 1}, {0, 0, 0}}, ecrontab_time_util:next_month({{2000, 1, 1}, {0, 0, 0}})),
        ?_assertEqual({{2000, 2, 29}, {0, 0, 0}}, ecrontab_time_util:next_month({{2000, 1, 31}, {0, 0, 0}})),
        ?_assertEqual({{2001, 2, 28}, {0, 0, 0}}, ecrontab_time_util:next_month({{2001, 1, 31}, {0, 0, 0}}))
    ].

next_day_test_() ->
    [

        ?_assertEqual({{1996, 1, 2}, {0, 0, 0}}, ecrontab_time_util:next_day({{1996, 1, 1}, {0, 0, 0}})),
        ?_assertEqual({{1996, 1, 31}, {0, 0, 0}}, ecrontab_time_util:next_day({{1996, 1, 30}, {0, 0, 0}})),
        ?_assertEqual({{1996, 2, 1}, {0, 0, 0}}, ecrontab_time_util:next_day({{1996, 1, 31}, {0, 0, 0}})),

        ?_assertEqual({{1996, 2, 2}, {0, 0, 0}}, ecrontab_time_util:next_day({{1996, 2, 1}, {0, 0, 0}})),
        ?_assertEqual({{1995, 3, 1}, {0, 0, 0}}, ecrontab_time_util:next_day({{1995, 2, 28}, {0, 0, 0}})),
        ?_assertEqual({{1996, 2, 29}, {0, 0, 0}}, ecrontab_time_util:next_day({{1996, 2, 28}, {0, 0, 0}})),
        ?_assertEqual({{1996, 3, 1}, {0, 0, 0}}, ecrontab_time_util:next_day({{1996, 2, 29}, {0, 0, 0}})),
        ?_assertEqual({{1996, 3, 1}, {0, 0, 0}}, ecrontab_time_util:next_day({{1996, 2, 29}, {0, 0, 0}})),
        ?_assertEqual({{1996, 3, 1}, {0, 0, 0}}, ecrontab_time_util:next_day({{1996, 2, 29}, {0, 0, 0}})),

        ?_assertEqual({{1996, 4, 1}, {0, 0, 0}}, ecrontab_time_util:next_day({{1996, 3, 31}, {0, 0, 0}})),
        ?_assertEqual({{1996, 5, 1}, {0, 0, 0}}, ecrontab_time_util:next_day({{1996, 4, 30}, {0, 0, 0}})),
        ?_assertEqual({{1996, 6, 1}, {0, 0, 0}}, ecrontab_time_util:next_day({1996, 5, 31})),
        ?_assertEqual({{1996, 7, 1}, {0, 0, 0}}, ecrontab_time_util:next_day({{1996, 6, 30}, {0, 0, 0}})),
        ?_assertEqual({{1996, 8, 1}, {0, 0, 0}}, ecrontab_time_util:next_day({{1996, 7, 31}, {0, 0, 0}})),
        ?_assertEqual({{1996, 9, 1}, {0, 0, 0}}, ecrontab_time_util:next_day({1996, 8, 31})),
        ?_assertEqual({{1996, 10, 1}, {0, 0, 0}}, ecrontab_time_util:next_day({{1996, 9, 30}, {0, 0, 0}})),
        ?_assertEqual({{1996, 11, 1}, {0, 0, 0}}, ecrontab_time_util:next_day({{1996, 10, 31}, {0, 0, 0}})),
        ?_assertEqual({{1996, 12, 1}, {0, 0, 0}}, ecrontab_time_util:next_day({1996, 11, 30})),
        ?_assertEqual({{1997, 1, 1}, {0, 0, 0}}, ecrontab_time_util:next_day({{1996, 12, 31}, {0, 0, 0}}))
    ].

next_hour_test_() ->
    [
        ?_assertEqual({{2000, 1, 1}, {23, 0, 0}}, ecrontab_time_util:next_hour({{2000, 1, 1}, {22, 0, 0}})),
        ?_assertEqual({{2000, 1, 2}, {0, 0, 0}}, ecrontab_time_util:next_hour({{2000, 1, 1}, {23, 0, 0}}))
    ].

next_minute_test_() ->
    [
        ?_assertEqual({{2000, 1, 1}, {0, 1, 0}}, ecrontab_time_util:next_minute({{2000, 1, 1}, {0, 0, 0}})),
        ?_assertEqual({{2000, 1, 1}, {1, 0, 0}}, ecrontab_time_util:next_minute({{2000, 1, 1}, {0, 59, 0}}))
    ].

next_second_test_() ->
    [
        ?_assertEqual({{2000, 1, 1}, {0, 0, 1}}, ecrontab_time_util:next_second({{2000, 1, 1}, {0, 0, 0}})),
        ?_assertEqual({{2000, 1, 1}, {0, 1, 0}}, ecrontab_time_util:next_second({{2000, 1, 1}, {0, 0, 59}}))
    ].