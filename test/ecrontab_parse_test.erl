-module(ecrontab_parse_test).
-include_lib("eunit/include/eunit.hrl").
-include("ecrontab.hrl").


all_test_() ->
    [
        any_test_list(),
        num_test_list(),
        list_test_list(),
        interval_test_list()
    ].

%% Year, Month, Day, Week, Hour, Minute, Second
%% [{Spec0,Spec},...]

any_test_list() ->
    [
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_EVERY_SECOND,value=none,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
                }},
            ecrontab_parse:parse_spec({'*','*','*','*','*','*','*'})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_EVERY_SECOND,value=none,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({<<"*">>,<<"*">>,<<"*">>,<<"*">>,<<"*">>,<<"*">>,<<"*">>})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_EVERY_SECOND,value=none,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({"*","*","*","*","*","*","*"}))
    ].

num_test_list() ->
    [
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.year,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 2016},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({2016,'*','*','*','*','*','*'})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.month,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 12},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*',12,'*','*','*','*','*'})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.day,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 6},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*','*',6,'*','*','*','*'})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.week,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 3},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*','*','*',3,'*','*','*'})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.hour,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 23},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*','*','*','*',23,'*','*'})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.minute,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 4},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*','*','*','*','*',4,'*'})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.second,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 55}
            }},
            ecrontab_parse:parse_spec({'*','*','*','*','*','*',55}))
    ].

list_test_list() ->
    [
        % year

        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.year,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [2016,2017,2018]},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({<<"2016-2018">>,'*','*','*','*','*','*'})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.year,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [2016,2018,2020,2022,2024]},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({<<"2016-2024/2">>,'*','*','*','*','*','*'})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.year,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [2016,2021,2026]},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({<<"2016-2028/5">>,'*','*','*','*','*','*'})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.year,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [2016,2017,2018]},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({[2016,2017,2018,2010],'*','*','*','*','*','*'})),

        % month

        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.month,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [6,7,8,9,10,11]},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*',<<"6-11">>,'*','*','*','*','*'})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.month,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [6,8,10]},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*',<<"6-11/2">>,'*','*','*','*','*'})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.month,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [6,11]},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*',<<"6-11/5">>,'*','*','*','*','*'})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.month,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [1,3,5,7,9,11]},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*',<<"*/2">>,'*','*','*','*','*'})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.month,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [1,3,5,7,9,11]},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*',<<"/2">>,'*','*','*','*','*'})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.month,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [6,7,8,10]},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*',[6,7,8,10],'*','*','*','*','*'})),

        % day

        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.day,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [6,7,8,9,10,11]},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*','*',<<"6-11">>,'*','*','*','*'})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.day,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [6,8,10]},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*','*',<<"6-11/2">>,'*','*','*','*'})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.day,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [6,11]},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*','*',<<"6-11/5">>,'*','*','*','*'})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.day,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31]},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*','*',<<"*/2">>,'*','*','*','*'})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.day,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31]},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*','*',<<"/2">>,'*','*','*','*'})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.day,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [6,7,8,10]},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*','*',[6,7,8,10],'*','*','*','*'})),

        % week

        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.week,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [1,2,3,4,5]},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*','*','*',<<"1-5">>,'*','*','*'})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.week,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [1,3,5,7]},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*','*','*',<<"1-7/2">>,'*','*','*'})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.week,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [1,6]},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*','*','*',<<"1-7/5">>,'*','*','*'})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.week,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [1,3,5,7]},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*','*','*',<<"*/2">>,'*','*','*'})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.week,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [1,3,5,7]},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*','*','*',<<"/2">>,'*','*','*'})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.week,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [6,7]},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*','*','*',[6,7],'*','*','*'})),

        % hour

        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.hour,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [6,7,8,9,10,11]},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*','*','*','*',<<"6-11">>,'*','*'})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.hour,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [6,8,10]},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*','*','*','*',<<"6-11/2">>,'*','*'})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.hour,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [6,11]},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*','*','*','*',<<"6-11/5">>,'*','*'})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.hour,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [0,2,4,6,8,10,12,14,16,18,20,22]},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*','*','*','*',<<"*/2">>,'*','*'})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.hour,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [0,2,4,6,8,10,12,14,16,18,20,22]},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*','*','*','*',<<"/2">>,'*','*'})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.hour,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [6,7,8,10]},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*','*','*','*',[6,7,8,10],'*','*'})),

        % minute

        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.minute,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [6,7,8,9,10,11]},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*','*','*','*','*',<<"6-11">>,'*'})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.minute,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [6,8,10]},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*','*','*','*','*',<<"6-11/2">>,'*'})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.minute,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [6,11]},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*','*','*','*','*',<<"6-11/5">>,'*'})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.minute,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [0,2,4,6,8,10,12,14,16,18,20,
                    22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58]},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*','*','*','*','*',<<"*/2">>,'*'})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.minute,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [0,2,4,6,8,10,12,14,16,18,20,
                    22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58]},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*','*','*','*','*',<<"/2">>,'*'})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.minute,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [6,7,8,10]},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*','*','*','*','*',[6,7,8,10],'*'})),

        % second

        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.second,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [6,7,8,9,10,11]}
            }},
            ecrontab_parse:parse_spec({'*','*','*','*','*','*',<<"6-11">>})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.second,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [6,8,10]}
            }},
            ecrontab_parse:parse_spec({'*','*','*','*','*','*',<<"6-11/2">>})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.second,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [6,11]}
            }},
            ecrontab_parse:parse_spec({'*','*','*','*','*','*',<<"6-11/5">>})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.second,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [0,2,4,6,8,10,12,14,16,18,20,
                    22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58]}
            }},
            ecrontab_parse:parse_spec({'*','*','*','*','*','*',<<"*/2">>})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.second,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [0,2,4,6,8,10,12,14,16,18,20,
                    22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58]}
            }},
            ecrontab_parse:parse_spec({'*','*','*','*','*','*',<<"/2">>})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.second,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [6,7,8,10]}
            }},
            ecrontab_parse:parse_spec({'*','*','*','*','*','*',[6,7,8,10]}))
    ].

interval_test_list() ->
    [

        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_INTERVAL_YEAR,value=2,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_INTERVAL, value = 2},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({<<"*/2">>,'*','*','*','*','*','*'})),
        ?_assertEqual(
            {ok,#spec{type=?SPEC_TYPE_INTERVAL_YEAR,value=2,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_INTERVAL, value = 2},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({<<"/2">>,'*','*','*','*','*','*'}))
    ].

%%specific_dates_data() ->
%%    [
%%        {
%%            ['*',               2,   29,     20,  0],
%%            [2012,              3,   1,      0,   0],
%%            [2016,              2,   29,     20,  0]
%%        },
%%        {
%%            ['*',               1,   friday, 0,   0],
%%            [2012,              1,   1,      1,   43],
%%            [2012,              1,   6,      0,   0]
%%        },
%%        {
%%            [2014,              9,   15,     '*', 20],
%%            [2013,              11,  28,     12,  3],
%%            [2014,              9,   15,     0,   20]
%%        },
%%        {
%%            ['*',               2,   '*',    0,   0],
%%            [2012,              2,   28,     0,   0],
%%            [2012,              2,   29,     0,   0]
%%        },
%%        {
%%            ['*',               '*', 30,     '*', '*'],
%%            [2012,              4,   30,     0,   59],
%%            [2012,              4,   30,     1,   0]
%%        },
%%        {
%%            [['*', 2010, 2012], '*', 12,     '*', 10],
%%            [2012,              1,   1,      15,  45],
%%            [2012,              1,   12,     0,   10]
%%        }
%%    ].

%%parse_spec_fail_test() ->
%%    {error, year}   = ecrontab_parse:parse_spec([foo, 2,  3,       4,  5]),
%%    {error, month}  = ecrontab_parse:parse_spec([1,   13, 3,       4,  5]),
%%    {error, day}    = ecrontab_parse:parse_spec([1,   2,  [blorg], 4,  5]),
%%    {error, hour}   = ecrontab_parse:parse_spec([1,   2,  3,       24, 5]),
%%    {error, minute} = ecrontab_parse:parse_spec([1,   2,  3,       4,  60]).

