-module(ecrontab_next_time_test).
-include_lib("eunit/include/eunit.hrl").
-include("ecrontab.hrl").

all_test_() ->
    NowDatetime = {{2016,3,7},{22,2,39}},
    NowTimestamp = 1457359359,
    List1 = any_list(NowDatetime,NowTimestamp),
    List2 = only_one_list(NowDatetime,NowTimestamp),
    io:format("List1:~p,List2:~p~n",[List1,List2]),
    [
        any_list(NowDatetime,NowTimestamp)
    ].

any_list(NowDatetime,NowTimestamp) ->
    [
        %{'*','*','*','*','*','*','*'}
        ?_assertEqual({ok,1457359360},
            ecrontab_next_time:next_time(#spec{type=?SPEC_TYPE_EVERY_SECOND,value=none,
                year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
                second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
            },NowDatetime,NowTimestamp))
    ].

only_one_list(NowDatetime,NowTimestamp) ->
    [
        %{2016,'*','*','*','*','*','*'}
        %{ok,1457359360}
        ecrontab_next_time:next_time(#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.year,
            year = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 2016},
            month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
            day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
            week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
            hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
            minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
            second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
        },NowDatetime,NowTimestamp),
        %{'*',12,'*','*','*','*','*'}
        %{ok,1481119359}
        ecrontab_next_time:next_time(#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.month,
            year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
            month = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 12},
            day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
            week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
            hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
            minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
            second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
        },NowDatetime,NowTimestamp),
        %{'*','*',6,'*','*','*','*'}
        %{ok,1459951359}
        ecrontab_next_time:next_time(#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.day,
            year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
            month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
            day = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 6},
            week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
            hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
            minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
            second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
        },NowDatetime,NowTimestamp),
        %{'*','*','*',3,'*','*','*'}
        %{ok,1457532159}
        ecrontab_next_time:next_time(#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.week,
            year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
            month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
            day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
            week = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 3},
            hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
            minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
            second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
        },NowDatetime,NowTimestamp),
        %{'*','*','*','*',23,'*','*'}
        %{ok,1457362959}
        ecrontab_next_time:next_time(#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.hour,
            year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
            month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
            day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
            week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
            hour = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 23},
            minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
            second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
        },NowDatetime,NowTimestamp),
        %{'*','*','*','*','*',4,'*'}
        %{ok,1457359479}
        ecrontab_next_time:next_time(#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.minute,
            year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
            month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
            day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
            week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
            hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
            minute = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 4},
            second = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}
        },NowDatetime,NowTimestamp),
        %{'*','*','*','*','*','*',55}
        %{ok,1457359375}
        ecrontab_next_time:next_time(#spec{type=?SPEC_TYPE_ONLY_ONE,value=#spec.second,
            year = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
            month = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
            day = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
            week = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
            hour = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
            minute = #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY},
            second = #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = 55}
        },NowDatetime,NowTimestamp)
    ].