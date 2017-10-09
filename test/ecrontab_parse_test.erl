-module(ecrontab_parse_test).
-include_lib("eunit/include/eunit.hrl").
-include("ecrontab_parse.hrl").
-include("ecrontab.hrl").

all_test_() ->
    [
        any_test_list(),
        num_test_list(),
        list_test_list(),
        interval_test_list(),
        timestamp_test_list(),
        fail_test_list()
    ].

%% Year, Month, Day, Week, Hour, Minute, Second
%% [{Spec0,Spec},...]

any_test_list() ->
    [
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_EVERY_SECOND, value = none,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*', '*', '*', '*', '*', '*', '*'})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_EVERY_SECOND, value = none,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({<<"*">>, <<"*">>, <<"*">>, <<"*">>, <<"*">>, <<"*">>, <<"*">>})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_EVERY_SECOND, value = none,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({"*", "*", "*", "*", "*", "*", "*"}))
    ].

num_test_list() ->
    [
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.year,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 3016},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({3016, '*', '*', '*', '*', '*', '*'})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.month,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 12},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*', [12], '*', '*', '*', '*', '*'})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.day,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 6},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*', '*', 6, '*', '*', '*', '*'})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.week,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 3},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*', '*', '*', 3, '*', '*', '*'})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.hour,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 23},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*', '*', '*', '*', 23, '*', '*'})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.minute,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 4},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*', '*', '*', '*', '*', 4, '*'})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.second,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 55}
            }},
            ecrontab_parse:parse_spec({'*', '*', '*', '*', '*', '*', 55}))
    ].

list_test_list() ->
    [
        % year

        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.year,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [3016, 3017, 3018]},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({<<"3016-3018">>, '*', '*', '*', '*', '*', '*'})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.year,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [3016, 3018, 3020, 3022, 3024]},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({<<"3016-3024/2">>, '*', '*', '*', '*', '*', '*'})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.year,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [3016, 3021, 3026]},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({<<"3016-3028/5">>, '*', '*', '*', '*', '*', '*'})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.year,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [3016, 3017, 3018]},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({[3016, 3017, 3018, 2010], '*', '*', '*', '*', '*', '*'})),

        % month

        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.month,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [6, 7, 8, 9, 10, 11]},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*', <<"6-11/1">>, '*', '*', '*', '*', '*'})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_NORMAL, value = none,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [6, 8, 10]},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
                    16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31]},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 0}
            }},
            ecrontab_parse:parse_spec({<<"6-11/2">>, <<"*/1">>, '*', '*', '*'})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.month,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [6, 11]},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*', <<"6-11/5">>, '*', '*', '*', '*', '*'})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.month,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [1, 3, 5, 7, 9, 11]},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*', <<"*/2">>, '*', '*', '*', '*', '*'})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.month,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [1, 3, 5, 7, 9, 11]},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*', <<"/2">>, '*', '*', '*', '*', '*'})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.month,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [6, 7, 8, 10]},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*', [6, 7, 8, 10], '*', '*', '*', '*', '*'})),

        % day

        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.day,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [1, 2, 3, 4, 5, 6, 23, 24, 25, 26, 27, 28, 29, 30, 31]},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*', '*', <<"23-6">>, '*', '*', '*', '*'})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.day,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [2, 4, 6, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31]},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*', '*', <<"11-6/2">>, '*', '*', '*', '*'})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.day,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 30},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*', '*', <<"30-1/5">>, '*', '*', '*', '*'})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.day,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [1, 2, 3, 6, 7, 8, 9]},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*', '*', <<"1,2,3,6-9">>, '*', '*', '*', '*'})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.day,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [4, 30]},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*', '*', <<"30-4/5">>, '*', '*', '*', '*'})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.day,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [6, 7, 8, 10]},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*', '*', [6, 7, 8, 10], '*', '*', '*', '*'})),

        % week

        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.week,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [1, 2, 3, 4, 5]},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*', '*', '*', <<"1-5">>, '*', '*', '*'})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.week,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [1, 3, 5, 7]},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*', '*', '*', <<"1-7/2">>, '*', '*', '*'})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.week,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [1, 6]},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*', '*', '*', <<"1-7/5">>, '*', '*', '*'})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.week,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [1, 3, 5, 7]},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*', '*', '*', <<"*/2">>, '*', '*', '*'})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.week,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [1, 3, 5, 7]},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*', '*', '*', <<"/2">>, '*', '*', '*'})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.week,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [6, 7]},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*', '*', '*', [6, 7], '*', '*', '*'})),

        % hour

        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.hour,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [0, 1, 2, 3, 22, 23]},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*', '*', '*', '*', <<"22-3">>, '*', '*'})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.hour,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [0, 2, 4, 6]},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*', '*', '*', '*', <<"-6/2">>, '*', '*'})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.hour,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [6, 11]},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*', '*', '*', '*', <<"6-11/5">>, '*', '*'})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.hour,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22]},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*', '*', '*', '*', <<"*/2">>, '*', '*'})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.hour,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22]},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*', '*', '*', '*', <<"/2">>, '*', '*'})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.hour,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [6, 7, 8, 10]},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*', '*', '*', '*', [6, 7, 8, 10], '*', '*'})),

        % minute

        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.minute,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [6, 7, 8, 9, 10, 11]},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*', '*', '*', '*', '*', <<"6-11">>, '*'})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.minute,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [6, 8, 10]},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*', '*', '*', '*', '*', <<"6-11/2">>, '*'})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.minute,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [6, 11]},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*', '*', '*', '*', '*', <<"6-11/5">>, '*'})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.minute,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20,
                    22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48, 50, 52, 54, 56, 58]},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*', '*', '*', '*', '*', <<"*/2">>, '*'})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.minute,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20,
                    22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48, 50, 52, 54, 56, 58]},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*', '*', '*', '*', '*', <<"/2">>, '*'})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.minute,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [6, 7, 8, 10]},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({'*', '*', '*', '*', '*', [6, 7, 8, 10], '*'})),

        % second

        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.second,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [6, 7, 8, 9, 10, 11]}
            }},
            ecrontab_parse:parse_spec({'*', '*', '*', '*', '*', '*', <<"6-11">>})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.second,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [6, 8, 10]}
            }},
            ecrontab_parse:parse_spec({'*', '*', '*', '*', '*', '*', <<"6-11/2">>})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.second,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [6, 11]}
            }},
            ecrontab_parse:parse_spec({'*', '*', '*', '*', '*', '*', <<"6-11/5">>})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.second,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20,
                    22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48, 50, 52, 54, 56, 58]}
            }},
            ecrontab_parse:parse_spec({'*', '*', '*', '*', '*', '*', <<"*/2">>})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.second,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20,
                    22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48, 50, 52, 54, 56, 58]}
            }},
            ecrontab_parse:parse_spec({'*', '*', '*', '*', '*', '*', <<"/2">>})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_ONLY_ONE, value = #spec.second,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = [6, 7, 8, 10]}
            }},
            ecrontab_parse:parse_spec({'*', '*', '*', '*', '*', '*', [6, 7, 8, 10]})),

        % other
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_NORMAL, value = none,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 2016},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 4},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 3},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 4},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 5},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 0}
            }},
            ecrontab_parse:parse_spec({2016, 4, '*', 3, 4, 5, 0}, []))
    ].

interval_test_list() ->
    [
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_INTERVAL_YEAR, value = 1,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_INTERVAL, value = 1},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({<<"*/1">>, '*', '*', '*', '*', '*', '*'})),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_INTERVAL_YEAR, value = 2,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_INTERVAL, value = 2},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            }},
            ecrontab_parse:parse_spec({<<"/2">>, '*', '*', '*', '*', '*', '*'}))
    ].

timestamp_test_list() ->
    [
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_TIMESTAMP, value = ?DATETIME_TO_TIMESTAMP({{2016, 4, 3}, {4, 5, 0}}),
                year = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 2016},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 4},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 3},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 4},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 5},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 0}
            }},
            ecrontab_parse:parse_spec({2016, 4, 3, '*', 4, 5, 0}, [])),
        ?_assertEqual(
            {ok, #spec{type = ?SPEC_TYPE_TIMESTAMP, value = ?DATETIME_TO_TIMESTAMP({{2016, 4, 3}, {4, 5, 0}}),
                year = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 2016},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 4},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 3},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 4},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 5},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 0}
            }},
            ecrontab_parse:parse_spec({2016, 4, 3, '*', 4, 5, 0}, [{filter_over_time, {{2016, 4, 3}, {4, 4, 0}}}]))
    ].

fail_test_list() ->
    [
        ?_assertEqual({error, time_over},
            ecrontab_parse:parse_spec([2015, '*', '*', '*', '*', '*', '*'])),
        ?_assertEqual({error, time_over},
            ecrontab_parse:parse_spec([2015, 2, 3, '*', 4, 5, 0])),
        ?_assertEqual({error, time_over},
            ecrontab_parse:parse_spec({3016, 4, 3, '*', 4, 5, 0}, [{filter_over_time, {{3016, 5, 3}, {4, 4, 0}}}])),
        ?_assertEqual({error, time_over},
            ecrontab_parse:parse_spec([2016, 2, 3, '*', 4, 5, 0])),
        ?_assertEqual({error, {year, invalid_value}},
            ecrontab_parse:parse_spec([foo, 2, 3, '*', 4, 5, 0])),
        ?_assertEqual({error, {year, list_interval}},
            ecrontab_parse:parse_spec([[<<"*/1">>], 2, 3, '*', 4, 5, 0])),
        ?_assertEqual({error, {year, invalid_step}},
            ecrontab_parse:parse_spec([<<"/*">>, 2, 3, '*', 4, 5, 0])),
        ?_assertEqual({error, {year, invalid_range}},
            ecrontab_parse:parse_spec([<<"-2017/2">>, 2, 3, '*', 4, 5, 0])),
        ?_assertEqual({error, {year, invalid_range}},
            ecrontab_parse:parse_spec([<<"2017-/2">>, 2, 3, '*', 4, 5, 0])),
        ?_assertEqual({error, {year, invalid_range}},
            ecrontab_parse:parse_spec([<<"2017/2">>, 2, 3, '*', 4, 5, 0])),
        ?_assertEqual({error, {year, year_range}},
            ecrontab_parse:parse_spec([<<"2200-2017/2">>, 2, 3, '*', 4, 5, 0])),

        ?_assertEqual({error, {month, invalid_range}},
            ecrontab_parse:parse_spec([<<"-">>, 3, '*', 4, 5])),
        ?_assertEqual({error, {month, neg_integer}},
            ecrontab_parse:parse_spec([<<"*/-13">>, 3, '*', 4, 5])),
        ?_assertEqual({error, {month, invalid_step}},
            ecrontab_parse:parse_spec([<<"*/*">>, 3, '*', 4, 5])),
        ?_assertEqual({error, {month, invalid_step}},
            ecrontab_parse:parse_spec([<<"*/">>, 3, '*', 4, 5])),
        ?_assertEqual({error, {month, any}},
            ecrontab_parse:parse_spec([<<"1-*">>, 3, '*', 4, 5])),
        ?_assertEqual({error, {month, same_range}},
            ecrontab_parse:parse_spec([<<"1-1">>, 3, '*', 4, 5])),
        ?_assertEqual({error, {month, any}},
            ecrontab_parse:parse_spec([<<"*-">>, 3, '*', 4, 5])),
        ?_assertEqual({error, {month, invalid_value}},
            ecrontab_parse:parse_spec([<<"13/1">>, 3, '*', 4, 5])),
        ?_assertEqual({error, {month, invalid_step}},
            ecrontab_parse:parse_spec([<<"1-3/*">>, 3, '*', 4, 5])),
        ?_assertEqual({error, {month, invalid_value}},
            ecrontab_parse:parse_spec([13, 3, '*', 4, 5])),

        ?_assertEqual({error, {day, invalid_value}},
            ecrontab_parse:parse_spec([2, [blorg], '*', 4, 5])),
        ?_assertEqual({error, {day, empty_list}},
            ecrontab_parse:parse_spec([2, [[]], '*', 4, 5])),
        ?_assertEqual({error, {day, list_any}},
            ecrontab_parse:parse_spec([2, ['*'], '*', 4, 5])),

        ?_assertEqual({error, {hour, invalid_value}},
            ecrontab_parse:parse_spec({2, 3, '*', 24, 5})),

        ?_assertEqual({error, {week, empty_binary}},
            ecrontab_parse:parse_spec({2, 3, <<>>, 23, 5})),
        ?_assertEqual({error, {week, empty_list}},
            ecrontab_parse:parse_spec({2, 3, [], 23, 5})),

        ?_assertEqual({error, {minute, invalid_value}},
            ecrontab_parse:parse_spec(2, 3, '*', 4, 60)),

        ?_assertEqual({error, {second, invalid_value}},
            ecrontab_parse:parse_spec([2016, 4, 3, '*', 4, 0, 60]))
    ].
