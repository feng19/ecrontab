-module(ecrontab_next_time).
-include("ecrontab.hrl").

-export([
    next_time/2, next_time/3
]).

%% ====================================================================
%% next_time
%% ====================================================================
next_time(Spec, NowDatetime) ->
	next_time(Spec, NowDatetime, ecrontab_time_util:datetime_to_timestamp(NowDatetime)).
-spec next_time(Spec :: spec(), NowDatetime :: calendar:datetime(), NowTimestamp :: non_neg_integer()) ->
    {ok, Timestamp :: non_neg_integer()}|{false, over}.
next_time(#spec{type = ?SPEC_TYPE_INTERVAL_YEAR, year = #spec_field{type = ?SPEC_FIELD_TYPE_INTERVAL,value = Interval}} = Spec,
    NowDatetime, NowTimestamp) ->
    {{LastYear,_,_},_} = NowDatetime,
    NextYear = LastYear+Interval,
    SpecField = #spec_field{type = ?SPEC_FIELD_TYPE_NUM,value = NextYear},
    {SpecType,SpecTypeValue} = ecrontab_parse:get_spec_type(Spec#spec{year = SpecField}),
    next_time(Spec#spec{type = SpecType, value = SpecTypeValue, year = SpecField},
        NowDatetime, NowTimestamp);
next_time(#spec{type=?SPEC_TYPE_EVERY_SECOND}, _, NowTimestamp) ->
    {ok, NowTimestamp+1};
next_time(#spec{type=?SPEC_TYPE_TIMESTAMP,value=Timestamp}, _, NowTimestamp) ->
    if
        NowTimestamp > Timestamp ->
            {false, over};
        true ->
            {ok, Timestamp}
    end;
next_time(#spec{type=?SPEC_TYPE_ONLY_ONE,value=N}=Spec, NowDatetime, NowTimestamp) ->
    next_time_only_one(N, Spec, NowDatetime, NowTimestamp);
next_time(Spec, NowDatetime, _) ->
    #spec{year = Year, month = Month, day = Day, week = Week,
        hour = Hour, minute = Minute, second = Second} = Spec,
    next_time_year(Year, Month, Day, Week, Hour, Minute, Second, NowDatetime).


%% ====================================================================
%% next_time_year
%% ====================================================================

next_time_year(#spec_field{type = ?SPEC_FIELD_TYPE_ANY}, Month, Day, Week, Hour, Minute, Second, NowDatetime) ->
    NowYear = ecrontab_time_util:get_datetime_year(NowDatetime),
    next_time_year_any(NowYear, Month, Day, Week, Hour, Minute, Second, NowDatetime);
next_time_year(#spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = Year}, Month, Day, Week, Hour, Minute, Second, NowDatetime) ->
    NowYear = ecrontab_time_util:get_datetime_year(NowDatetime),
    if
        Year < NowYear ->
            {false, over};
        true ->
            case next_time_month(Year, Month, Day, Week, Hour, Minute, Second, NowDatetime) of
                false ->
                    {false, over};
                Result ->
                    Result
            end
    end;
next_time_year(#spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = List}, Month, Day, Week, Hour, Minute, Second, NowDatetime) ->
    NowYear = ecrontab_time_util:get_datetime_year(NowDatetime),
    case lists:member(NowYear, List) of
        true ->
            case next_time_month(NowYear, Month, Day, Week, Hour, Minute, Second, NowDatetime) of
                false ->
                    next_time_year_list(NowYear, List, Month, Day, Week, Hour, Minute, Second, NowDatetime);
                Result ->
                    Result
            end;
        false ->
            next_time_year_list(NowYear, List, Month, Day, Week, Hour, Minute, Second, NowDatetime)
    end.

next_time_year_any(NowYear, Month, Day, Week, Hour, Minute, Second, NowDatetime) ->
    case next_time_month(NowYear, Month, Day, Week, Hour, Minute, Second, NowDatetime) of
        false ->
            next_time_year_any(NowYear+1, Month, Day, Week, Hour, Minute, Second, NowDatetime);
        Result ->
            Result
    end.

next_time_year_list(NowYear, List, Month, Day, Week, Hour, Minute, Second, NowDatetime) ->
    case find_next_in_list(NowYear, List) of
        false ->
            {false, over};
        NextYear ->
            case next_time_month(NextYear, Month, Day, Week, Hour, Minute, Second, NowDatetime) of
                false ->
                    next_time_year_list(NextYear, List, Month, Day, Week, Hour, Minute, Second, NowDatetime);
                Result ->
                    Result
            end
    end.

%% ====================================================================
%% next_time_month
%% ====================================================================
next_time_month(Year, #spec_field{type = ?SPEC_FIELD_TYPE_ANY}, Day, Week, Hour, Minute, Second, NowDatetime) ->
    case NowDatetime of
        {{Year,NowMonth,_},_} ->
            next_time_month_any(Year, NowMonth, Day, Week, Hour, Minute, Second, NowDatetime);
        _ ->
            next_time_month_any(Year, 1, Day, Week, Hour, Minute, Second, NowDatetime)
    end;
next_time_month(Year, #spec_field{type = ?SPEC_FIELD_TYPE_NUM,value = Month}, Day, Week, Hour, Minute, Second, NowDatetime) ->
    case NowDatetime of
        {{Year,NowMonth,_},_} when NowMonth < Month ->
            false;
        _ ->
            next_time_day_or_week(Year, Month, Day, Week, Hour, Minute, Second, NowDatetime)
    end;
next_time_month(Year, #spec_field{type = ?SPEC_FIELD_TYPE_LIST,value = List}, Day, Week, Hour, Minute, Second, NowDatetime) ->
    case NowDatetime of
        {{Year,NowMonth,_},_} ->
            case lists:member(NowMonth, List) of
                true ->
                    case next_time_day_or_week(Year, NowMonth, Day, Week, Hour, Minute, Second, NowDatetime) of
                        false ->
                            next_time_month_list(Year, NowMonth, List, Day, Week, Hour, Minute, Second, NowDatetime);
                        Result ->
                            Result
                    end;
                false ->
                    next_time_month_list(Year, NowMonth, List, Day, Week, Hour, Minute, Second, NowDatetime)
            end;
        _ ->
            NextMonth = hd(List),
            case next_time_day_or_week(Year, NextMonth, Day, Week, Hour, Minute, Second, NowDatetime) of
                false ->
                    next_time_month_list(Year, NextMonth, List, Day, Week, Hour, Minute, Second, NowDatetime);
                Result ->
                    Result
            end
    end.

next_time_month_any(Year, NowMonth, Day, Week, Hour, Minute, Second, NowDatetime) ->
    case next_time_day_or_week(Year, NowMonth, Day, Week, Hour, Minute, Second, NowDatetime) of
        false ->
            case NowMonth of
                12 ->
                    false;
                _ ->
                    next_time_month_any(Year, NowMonth+1, Day, Week, Hour, Minute, Second, NowDatetime)
            end;
        Result ->
            Result
    end.

next_time_month_list(Year, NowMonth, List, Day, Week, Hour, Minute, Second, NowDatetime) ->
    case find_next_in_list(NowMonth, List) of
        false ->
            false;
        NextMonth ->
            case next_time_day_or_week(Year, NextMonth, Day, Week, Hour, Minute, Second, NowDatetime) of
                false ->
                    next_time_month_list(Year, NextMonth, List, Day, Week, Hour, Minute, Second, NowDatetime);
                Result ->
                    Result
            end
    end.

%% ====================================================================
%% next_time_day_or_week
%% ====================================================================
next_time_day_or_week(Year, Month, Day, Week, Hour, Minute, Second, NowDatetime) ->
    case {Day#spec_field.type, Week#spec_field.type} of
        {_, ?SPEC_FIELD_TYPE_ANY} ->
            next_time_day(Year, Month, Day, Hour, Minute, Second, NowDatetime);
        {?SPEC_FIELD_TYPE_ANY, ?SPEC_FIELD_TYPE_ANY} ->
            next_time_day(Year, Month, Day, Hour, Minute, Second, NowDatetime);
        {?SPEC_FIELD_TYPE_ANY, _} ->
            next_time_week(Year, Month, Week, Hour, Minute, Second, NowDatetime);
        _ ->
            case {next_time_day(Year, Month, Day, Hour, Minute, Second, NowDatetime),
                next_time_week(Year, Month, Week, Hour, Minute, Second, NowDatetime)} of
                {false, false} ->
                    false;
                {false, Result} ->
                    Result;
                {Result, false} ->
                    Result;
                {{ok, Timestamp1}, {ok, Timestamp2}} when Timestamp1 < Timestamp2 ->
                    {ok, Timestamp1};
                {_, Result} ->
                    Result
            end
    end.

%% ====================================================================
%% next_time_day
%% ====================================================================
next_time_day(Year, Month, #spec_field{type = ?SPEC_FIELD_TYPE_ANY}, Hour, Minute, Second, NowDatetime) ->
    case NowDatetime of
        {{Year,Month,NowDay},_} ->
            next_time_day_any(Year, Month, NowDay, Hour, Minute, Second, NowDatetime);
        _ ->
            next_time_day_any(Year, Month, 1, Hour, Minute, Second, NowDatetime)
    end;
next_time_day(Year, Month, #spec_field{type = ?SPEC_FIELD_TYPE_NUM,value = Day}, Hour, Minute, Second, NowDatetime) ->
    case NowDatetime of
        {{Year,Month,NowDay},_} when NowDay > Day ->
            false;
        _ ->
            Date = {Year, Month, Day},
            next_time_hour(Date, Hour, Minute, Second, NowDatetime)
    end;
next_time_day(Year, Month, #spec_field{type = ?SPEC_FIELD_TYPE_LIST,value = List}, Hour, Minute, Second, NowDatetime) ->
    case NowDatetime of
        {{Year,Month,NowDay}=NowDate,_} ->
            case lists:member(NowDay, List) of
                true ->
                    case next_time_hour(NowDate, Hour, Minute, Second, NowDatetime) of
                        false ->
                            next_time_day_list(Year, Month, NowDay, List, Hour, Minute, Second, NowDatetime);
                        Result ->
                            Result
                    end;
                false ->
                    next_time_day_list(Year, Month, NowDay, List, Hour, Minute, Second, NowDatetime)
            end;
        _ ->
            NextDay = hd(List),
            case next_time_hour({Year, Month, NextDay}, Hour, Minute, Second, NowDatetime) of
                false ->
                    next_time_day_list(Year, Month, NextDay, List, Hour, Minute, Second, NowDatetime);
                Result ->
                    Result
            end
    end.

next_time_day_any(Year, Month, NowDay, Hour, Minute, Second, NowDatetime) ->
    case next_time_hour({Year, Month, NowDay}, Hour, Minute, Second, NowDatetime) of
        false ->
            case calendar:last_day_of_the_month(Year, Month) of
                NowDay ->
                    false;
                _ ->
                    next_time_day_any(Year, Month, NowDay, Hour, Minute, Second, NowDatetime)
            end;
        Result ->
            Result
    end.

next_time_day_list(Year, Month, NowDay, List, Hour, Minute, Second, NowDatetime) ->
    case find_next_in_list(NowDay, List) of
        false ->
            false;
        NextDay ->
            case next_time_hour({Year, Month, NextDay}, Hour, Minute, Second, NowDatetime) of
                false ->
                    next_time_day_list(Year, Month, NextDay, List, Hour, Minute, Second, NowDatetime);
                Result ->
                    Result
            end
    end.

%% ====================================================================
%% next_time_week
%% ====================================================================
next_time_week(Year, Month, #spec_field{type = ?SPEC_FIELD_TYPE_NUM,value = Week}, Hour, Minute, Second, NowDatetime) ->
    case NowDatetime of
        {{Year,Month,NowDay}=NowDate,_} ->
            case calendar:day_of_the_week(Year, Month, NowDay) of
                Week ->
                    next_time_hour(NowDate, Hour, Minute, Second, NowDatetime);
                _ ->
                    next_time_week_num(Year, Month, NowDay, Week, Hour, Minute, Second, NowDatetime)
            end;
        _ ->
            case calendar:day_of_the_week(Year, Month, 1) of
                Week ->
                    next_time_hour({Year, Month, 1}, Hour, Minute, Second, NowDatetime);
                _ ->
                    next_time_week_num(Year, Month, 1, Week, Hour, Minute, Second, NowDatetime)
            end
    end;
next_time_week(Year, Month, #spec_field{type = ?SPEC_FIELD_TYPE_LIST,value = List}, Hour, Minute, Second, NowDatetime) ->
    LastDay = calendar:last_day_of_the_month(Year, Month),
    DateWeekList =
        case NowDatetime of
            {{Year,Month,NowDay},_} ->
                Week = calendar:day_of_the_week(Year, Month, NowDay),
                get_date_week_list(Year, Month, NowDay, LastDay, Week);
            _ ->
                Week = calendar:day_of_the_week(Year, Month, 1),
                get_date_week_list(Year, Month, 1, LastDay, Week)
        end,
    next_time_week_list(DateWeekList, List, Hour, Minute, Second, NowDatetime).

next_time_week_num(Year, Month, Day, Week, Hour, Minute, Second, NowDatetime) ->
    case get_next_week_date(Week, Year, Month, Day) of
        false ->
            false;
        Date ->
            case next_time_hour(Date, Hour, Minute, Second, NowDatetime) of
                false ->
                    next_time_week_num(Year, Month, Day, Week, Hour, Minute, Second, NowDatetime);
                Result ->
                    Result
            end
    end.

get_date_week_list(Year, Month, LastDay, LastDay, Week) ->
    [{{Year, Month, LastDay},Week}];
get_date_week_list(Year, Month, Day, LastDay, 7) ->
    [{{Year, Month, LastDay},7}|get_date_week_list(Year, Month, Day+1, LastDay, 1)];
get_date_week_list(Year, Month, Day, LastDay, Week) ->
    [{{Year, Month, LastDay},Week}|get_date_week_list(Year, Month, Day+1, LastDay, Week+1)].

next_time_week_list([], _, _, _, _, _) ->
    false;
next_time_week_list([{Date,Week}| DateWeekList], List, Hour, Minute, Second, NowDatetime) ->
    case lists:member(Week, List) of
        true ->
            case next_time_hour(Date, Hour, Minute, Second, NowDatetime) of
                false ->
                    next_time_week_list(DateWeekList, List, Hour, Minute, Second, NowDatetime);
                Result ->
                    Result
            end;
        false ->
            next_time_week_list(DateWeekList, List, Hour, Minute, Second, NowDatetime)
    end.

get_next_week_date(Week, Year, Month, Day) ->
    NowWeek = calendar:day_of_the_week(Year, Month, Day),
    DiffDays =
        if
            NowWeek == Week ->
                7;
            NowWeek > Week ->
                7 - (NowWeek - Week);
            true ->
                Week - NowWeek
        end,
    NowDays = calendar:date_to_gregorian_days(Year, Month, Day),
    NewDays = NowDays + DiffDays,
    case calendar:gregorian_days_to_date(NewDays) of
        {Year,Month,_} = NewDate ->
            NewDate;
        _ ->
            false
    end.

%% ====================================================================
%% next_time_hour
%% ====================================================================
next_time_hour(Date, #spec_field{type = ?SPEC_FIELD_TYPE_ANY}, Minute, Second, NowDatetime) ->
    case NowDatetime of
        {Date, {NowHour,_,_}} ->
            next_time_hour_any(Date, NowHour, Minute, Second, NowDatetime);
        _ ->
            next_time_hour_any(Date, 0, Minute, Second, NowDatetime)
    end;
next_time_hour(Date, #spec_field{type = ?SPEC_FIELD_TYPE_NUM,value = Hour}, Minute, Second, NowDatetime) ->
    case NowDatetime of
        {Date, {NowHour,_,_}} when NowHour > Hour ->
            false;
        _ ->
            next_time_minute(Date, Hour, Minute, Second, NowDatetime)
    end;
next_time_hour(Date, #spec_field{type = ?SPEC_FIELD_TYPE_LIST,value = List}, Minute, Second, NowDatetime) ->
    case NowDatetime of
        {Date,{NowHour,_,_}} ->
            case lists:member(NowHour, List) of
                true ->
                    case next_time_minute(Date, NowHour, Minute, Second, NowDatetime) of
                        false ->
                            next_time_hour_list(Date, NowHour, List, Minute, Second, NowDatetime);
                        Result ->
                            Result
                    end;
                false ->
                    next_time_hour_list(Date, NowHour, List, Minute, Second, NowDatetime)
            end;
        _ ->
            NextHour = hd(List),
            case next_time_minute(Date, NextHour, Minute, Second, NowDatetime) of
                false ->
                    next_time_hour_list(Date, NextHour, List, Minute, Second, NowDatetime);
                Result ->
                    Result
            end
    end.

next_time_hour_any(Date, NowHour, Minute, Second, NowDatetime) ->
    case next_time_minute(Date, NowHour, Minute, Second, NowDatetime) of
        false ->
            case NowHour of
                23 ->
                    false;
                _ ->
                    next_time_hour_any(Date, NowHour+1, Minute, Second, NowDatetime)
            end;
        Result ->
            Result
    end.

next_time_hour_list(Date, NowHour, List, Minute, Second, NowDatetime) ->
    case find_next_in_list(NowHour, List) of
        false ->
            false;
        NextHour ->
            case next_time_minute(Date, NextHour, Minute, Second, NowDatetime) of
                false ->
                    next_time_hour_list(Date, NowHour, List, Minute, Second, NowDatetime);
                Result ->
                    Result
            end
    end.

%% ====================================================================
%% next_time_minute
%% ====================================================================
next_time_minute(Date, Hour, #spec_field{type = ?SPEC_FIELD_TYPE_ANY}, Second, NowDatetime) ->
    case NowDatetime of
        {Date,{Hour,NowMinute,_}} ->
            next_time_minute_any(Date, Hour, NowMinute, Second, NowDatetime);
        _ ->
            next_time_minute_any(Date, Hour, 0, Second, NowDatetime)
    end;
next_time_minute(Date, Hour, #spec_field{type = ?SPEC_FIELD_TYPE_NUM,value = Minute}, Second, NowDatetime) ->
    case NowDatetime of
        {Date,{Hour,NowMinute,_}} when NowMinute > Minute ->
            false;
        _ ->
            next_time_second(Date, Hour, Minute, Second, NowDatetime)
    end;
next_time_minute(Date, Hour, #spec_field{type = ?SPEC_FIELD_TYPE_LIST,value = List}, Second, NowDatetime) ->
    case NowDatetime of
        {Date,{Hour,NowMinute,_}} ->
            case lists:member(NowMinute,List) of
                true ->
                    case next_time_second(Date, Hour, NowMinute, Second, NowDatetime) of
                        false ->
                            next_time_minute_list(Date, Hour, NowMinute, List, Second, NowDatetime);
                        Result ->
                            Result
                    end;
                false ->
                    next_time_minute_list(Date, Hour, NowMinute, List, Second, NowDatetime)
            end;
        _ ->
            NextMinute = hd(List),
            case next_time_second(Date, Hour, NextMinute, Second, NowDatetime) of
                false ->
                    next_time_minute_list(Date, Hour, NextMinute, List, Second, NowDatetime);
                Result ->
                    Result
            end
    end.

next_time_minute_any(Date, Hour, NowMinute, Second, NowDatetime) ->
    case next_time_second(Date, Hour, NowMinute, Second, NowDatetime) of
        false ->
            case NowMinute of
                59 ->
                    false;
                _ ->
                    next_time_minute_any(Date, Hour, NowMinute+1, Second, NowDatetime)
            end;
        Result ->
            Result
    end.

next_time_minute_list(Date, Hour, NowMinute, List, Second, NowDatetime) ->
    case find_next_in_list(NowMinute,List) of
        false ->
            false;
        NextMinute ->
            case next_time_second(Date, Hour, NextMinute, Second, NowDatetime) of
                false ->
                    next_time_minute_list(Date, Hour, NextMinute, List, Second, NowDatetime);
                Result ->
                    Result
            end
    end.

%% ====================================================================
%% next_time_second
%% ====================================================================
next_time_second(Date, Hour, Minute, #spec_field{type = ?SPEC_FIELD_TYPE_ANY}, NowDatetime) ->
    case NowDatetime of
        {Date,{Hour,Minute,NowSecond}} ->
            next_time_second_any(Date, Hour, Minute, NowSecond, NowDatetime);
        _ ->
            next_time_second_any(Date, Hour, Minute, 0, NowDatetime)
    end;
next_time_second(Date, Hour, Minute, #spec_field{type = ?SPEC_FIELD_TYPE_NUM,value = Second}, NowDatetime) ->
    case NowDatetime of
        {Date,{Hour,Minute,NowSecond}} when NowSecond > Second ->
            false;
        _ ->
            Timestamp = ecrontab_time_util:datetime_to_timestamp({Date, {Hour, Minute, Second}}),
            {ok, Timestamp}
    end;
next_time_second(Date, Hour, Minute, #spec_field{type = ?SPEC_FIELD_TYPE_LIST,value = List}, NowDatetime) ->
    case NowDatetime of
        {Date,{Hour,Minute,NowSecond}} ->
            next_time_second_list(Date, Hour, Minute, NowSecond, List, NowDatetime);
        _ ->
            NextSecond = hd(List),
            Datetime = {Date, {Hour,Minute,NextSecond}},
            if
                Datetime > NowDatetime ->
                    {ok, ecrontab_time_util:datetime_to_timestamp(Datetime)};
                true ->
                    next_time_second_list(Date, Hour, Minute, NextSecond, List, NowDatetime)
            end
    end.

next_time_second_any(Date, Hour, Minute, NowSecond, NowDatetime) ->
    Datetime = {Date, {Hour,Minute,NowSecond}},
    if
        Datetime > NowDatetime ->
            {ok, ecrontab_time_util:datetime_to_timestamp(Datetime)};
        true ->
            case NowSecond of
                59 ->
                    false;
                _ ->
                    next_time_second_any(Date, Hour, Minute, NowSecond+1, NowDatetime)
            end
    end.

next_time_second_list(Date, Hour, Minute, NowSecond, List, NowDatetime) ->
    case find_next_in_list(NowSecond,List) of
        false ->
            false;
        NextSecond ->
            Datetime = {Date, {Hour,Minute,NextSecond}},
            if
                Datetime > NowDatetime ->
                    {ok, ecrontab_time_util:datetime_to_timestamp(Datetime)};
                true ->
                    next_time_second_list(Date, Hour, Minute, NextSecond, List, NowDatetime)
            end
    end.

%% ====================================================================
%% next_time_only_one start
%% ====================================================================

next_time_only_one(#spec.year,Spec,NowDatetime,NowTimestamp) ->
    SpecField = Spec#spec.year,
    NowYear = ecrontab_time_util:get_datetime_year(NowDatetime),
    case is_passed_spec_field(NowYear, SpecField) of
        true ->
            Timestamp = NowTimestamp+1,
            NewDatetime = ecrontab_time_util:timestamp_to_datetime(Timestamp),
            case ecrontab_time_util:get_datetime_year(NewDatetime) of
                NowYear ->
                    {ok, Timestamp};
                _ ->
                    next_time_only_one_year(SpecField,NowDatetime)
            end;
        false ->
            next_time_only_one_year(SpecField,NowDatetime)
    end;

next_time_only_one(#spec.month,Spec,NowDatetime,NowTimestamp) ->
    SpecField = Spec#spec.month,
    NowMonth = ecrontab_time_util:get_datetime_month(NowDatetime),
    case is_passed_spec_field(NowMonth, SpecField) of
        true ->
            Timestamp = NowTimestamp+1,
            NewDatetime = ecrontab_time_util:timestamp_to_datetime(Timestamp),
            case ecrontab_time_util:get_datetime_month(NewDatetime) of
                NowMonth ->
                    {ok, Timestamp};
                _ ->
                    next_time_only_one_month(SpecField,NowDatetime)
            end;
        false ->
            next_time_only_one_month(SpecField,NowDatetime)
    end;
next_time_only_one(#spec.day,Spec,NowDatetime,NowTimestamp) ->
    SpecField = Spec#spec.day,
    NowDay = ecrontab_time_util:get_datetime_day(NowDatetime),
    case is_passed_spec_field(NowDay, SpecField) of
        true ->
            Timestamp = NowTimestamp+1,
            NewDatetime = ecrontab_time_util:timestamp_to_datetime(Timestamp),
            case ecrontab_time_util:get_datetime_day(NewDatetime) of
                NowDay ->
                    {ok, Timestamp};
                _ ->
                    next_time_only_one_day(SpecField,NowDatetime)
            end;
        false ->
            next_time_only_one_day(SpecField,NowDatetime)
    end;
next_time_only_one(#spec.week,Spec,NowDatetime,NowTimestamp) ->
    NowWeek = calendar:day_of_the_week(element(1,NowDatetime)),
    SpecField = Spec#spec.week,
    case is_passed_spec_field(NowWeek, SpecField) of
        true ->
            Timestamp = NowTimestamp+1,
            NewDatetime = ecrontab_time_util:timestamp_to_datetime(Timestamp),
            case ecrontab_time_util:get_datetime_week(NewDatetime) of
                NowWeek ->
                    {ok, Timestamp};
                _ ->
                    next_time_only_one_week(SpecField,NowWeek,NowDatetime)
            end;
        false ->
            next_time_only_one_week(SpecField,NowWeek,NowDatetime)
    end;
next_time_only_one(#spec.hour,Spec,NowDatetime,NowTimestamp) ->
    SpecField = Spec#spec.hour,
    NowHour = ecrontab_time_util:get_datetime_hour(NowDatetime),
    case is_passed_spec_field(NowHour, SpecField) of
        true ->
            Timestamp = NowTimestamp+1,
            NewDatetime = ecrontab_time_util:timestamp_to_datetime(Timestamp),
            case ecrontab_time_util:get_datetime_hour(NewDatetime) of
                NowHour ->
                    {ok, Timestamp};
                _ ->
                    next_time_only_one_hour(SpecField,NowDatetime)
            end;
        false ->
            next_time_only_one_hour(SpecField,NowDatetime)
    end;
next_time_only_one(#spec.minute,Spec,NowDatetime,NowTimestamp) ->
    SpecField = Spec#spec.minute,
    NowMinute = ecrontab_time_util:get_datetime_minute(NowDatetime),
    case is_passed_spec_field(NowMinute, SpecField) of
        true ->
            Timestamp = NowTimestamp+1,
            NewDatetime = ecrontab_time_util:timestamp_to_datetime(Timestamp),
            case ecrontab_time_util:get_datetime_minute(NewDatetime) of
                NowMinute ->
                    {ok, Timestamp};
                _ ->
                    next_time_only_one_minute(SpecField,NowDatetime)
            end;
        false ->
            next_time_only_one_minute(SpecField,NowDatetime)
    end;
next_time_only_one(#spec.second,Spec,NowDatetime,NowTimestamp) ->
    SpecField = Spec#spec.second,
    NowSecond = ecrontab_time_util:get_datetime_minute(NowDatetime),
    case is_passed_spec_field(NowSecond, SpecField) of
        true ->
            Timestamp = NowTimestamp+1,
            NewDatetime = ecrontab_time_util:timestamp_to_datetime(Timestamp),
            case ecrontab_time_util:get_datetime_second(NewDatetime) of
                NowSecond ->
                    {ok, Timestamp};
                _ ->
                    next_time_only_one_second(SpecField,NowDatetime)
            end;
        false ->
            next_time_only_one_second(SpecField,NowDatetime)
    end.

next_time_only_one_year(SpecField,{{NowYear,M,D},Time}) ->
    case SpecField#spec_field.type of
        ?SPEC_FIELD_TYPE_NUM ->
            if
                NowYear > SpecField#spec_field.value ->
                    {false, over};
                true ->
                    {ok, ecrontab_time_util:datetime_to_timestamp({{SpecField#spec_field.value,M,D},Time})}
            end;
        ?SPEC_FIELD_TYPE_LIST ->
            case find_next_in_list(NowYear,SpecField#spec.value) of
                false ->
                    {false, over};
                NextYear ->
                    {ok, ecrontab_time_util:datetime_to_timestamp({{NextYear,M,D},Time})}
            end
    end.

next_time_only_one_month(SpecField,{{Y,NowMonth,D},Time}) ->
    case SpecField#spec_field.type of
        ?SPEC_FIELD_TYPE_NUM ->
            NextMonth = SpecField#spec_field.value,
            if
                NowMonth > NextMonth ->
                    next_time_only_one_month_do(Y+1,NextMonth,D,Time);
                true ->
                    next_time_only_one_month_do(Y,NextMonth,D,Time)
            end;
        ?SPEC_FIELD_TYPE_LIST ->
            case find_next_in_list(NowMonth,SpecField#spec.value) of
                false ->
                    NextMonth = hd(SpecField#spec.value),
                    next_time_only_one_month_do(Y+1,NextMonth,D,Time);
                NextMonth ->
                    next_time_only_one_month_do(Y,NextMonth,D,Time)
            end
    end.

next_time_only_one_month_do(Y,NextMonth,D,Time) ->
    LastDay = calendar:last_day_of_the_month(Y,NextMonth),
    if
        LastDay < D ->
            {ok, ecrontab_time_util:datetime_to_timestamp({{Y,NextMonth,LastDay},Time})};
        true ->
            {ok, ecrontab_time_util:datetime_to_timestamp({{Y,NextMonth,D},Time})}
    end.

next_time_only_one_day(SpecField,{{Y,M,NowDay},Time}) ->
    case SpecField#spec_field.type of
        ?SPEC_FIELD_TYPE_NUM ->
            NextDay = SpecField#spec_field.value,
            if
                NowDay > NextDay ->
                    case M of
                        12 ->
                            next_time_only_one_day_do(Y+1,1,NextDay,Time);
                        _ ->
                            next_time_only_one_day_do(Y,M+1,NextDay,Time)
                    end;
                true ->
                    next_time_only_one_day_do(Y,M,NextDay,Time)
            end;
        ?SPEC_FIELD_TYPE_LIST ->
            case find_next_in_list(NowDay,SpecField#spec.value) of
                false ->
                    NextDay = hd(SpecField#spec.value),
                    case M of
                        12 ->
                            next_time_only_one_day_do(Y+1,1,NextDay,Time);
                        _ ->
                            next_time_only_one_day_do(Y,M,NextDay,Time)
                    end;
                NextDay ->
                    LastDay = calendar:last_day_of_the_month(Y,M),
                    if
                        LastDay < NextDay ->
                            NextDay2 = hd(SpecField#spec.value),
                            case M of
                                12 ->
                                    next_time_only_one_day_do(Y+1,1,NextDay2,Time);
                                _ ->
                                    next_time_only_one_day_do(Y,M+1,NextDay2,Time)
                            end;
                        true ->
                            {ok, ecrontab_time_util:datetime_to_timestamp({{Y,M,NextDay},Time})}
                    end
            end
    end.

next_time_only_one_day_do(Y,M,NextDay,Time) ->
    LastDay = calendar:last_day_of_the_month(Y,M),
    if
        LastDay < NextDay ->
            case M of
                12 ->
                    next_time_only_one_day_do(Y+1,1,NextDay,Time);
                _ ->
                    next_time_only_one_day_do(Y,M+1,NextDay,Time)
            end;
        true ->
            {ok, ecrontab_time_util:datetime_to_timestamp({{Y,M,NextDay},Time})}
    end.

next_time_only_one_week(SpecField,NowWeek,{Date,Time}) ->
    Days = calendar:date_to_gregorian_days(Date),
    NewDays =
    case SpecField#spec_field.type of
        ?SPEC_FIELD_TYPE_NUM ->
            NextWeek = SpecField#spec_field.value,
            if
                NowWeek > NextWeek ->
                    Days + (7 - (NowWeek - NextWeek));
                true ->
                    Days + NextWeek - NowWeek
            end;
        ?SPEC_FIELD_TYPE_LIST ->
            case find_next_in_list(NowWeek,SpecField#spec.value) of
                false ->
                    NextWeek = hd(SpecField#spec.value),
                    Days + (7 - (NowWeek - NextWeek));
                NextWeek ->
                    Days + NextWeek - NowWeek
            end
    end,
    NewDate = calendar:gregorian_days_to_date(NewDays),
    {ok,ecrontab_time_util:datetime_to_timestamp({NewDate,Time})}.

next_time_only_one_hour(SpecField,{Date,{NowHour,MM,SS}}) ->
    DateTime =
    case SpecField#spec_field.type of
        ?SPEC_FIELD_TYPE_NUM ->
            NextHour = SpecField#spec_field.value,
            if
                NowHour > NextHour ->
                    ecrontab_time_util:next_day({Date,{NextHour,MM,SS}});
                true ->
                    {Date,{NextHour,MM,SS}}
            end;
        ?SPEC_FIELD_TYPE_LIST ->
            case find_next_in_list(NowHour,SpecField#spec.value) of
                false ->
                    NextHour = hd(SpecField#spec.value),
                    ecrontab_time_util:next_day({Date,{NextHour,MM,SS}});
                NextHour ->
                    {Date,{NextHour,MM,SS}}
            end
    end,
    {ok, ecrontab_time_util:datetime_to_timestamp(DateTime)}.

next_time_only_one_minute(SpecField,{Date,{HH,NowMinute,SS}}) ->
    DateTime =
    case SpecField#spec_field.type of
        ?SPEC_FIELD_TYPE_NUM ->
            NextMinute = SpecField#spec_field.value,
            if
                NowMinute > NextMinute ->
                    ecrontab_time_util:next_hour({Date,{HH,NextMinute,SS}});
                true ->
                    {Date,{HH,NextMinute,SS}}
            end;
        ?SPEC_FIELD_TYPE_LIST ->
            case find_next_in_list(NowMinute,SpecField#spec.value) of
                false ->
                    NextMinute = hd(SpecField#spec.value),
                    ecrontab_time_util:next_hour({Date,{HH,NextMinute,SS}});
                NextMinute ->
                    {Date,{HH,NextMinute,SS}}
            end
    end,
    {ok, ecrontab_time_util:datetime_to_timestamp(DateTime)}.

next_time_only_one_second(SpecField,{Date,{HH,MM,NowSecond}}) ->
    DateTime =
    case SpecField#spec_field.type of
        ?SPEC_FIELD_TYPE_NUM ->
            NextSecond = SpecField#spec_field.value,
            if
                NowSecond > NextSecond ->
                    ecrontab_time_util:next_minute({Date,{HH,MM,NextSecond}});
                true ->
                    {Date,{HH,MM,NextSecond}}
            end;
        ?SPEC_FIELD_TYPE_LIST ->
            case find_next_in_list(NowSecond,SpecField#spec.value) of
                false ->
                    NextSecond = hd(SpecField#spec.value),
                    ecrontab_time_util:next_minute({Date,{HH,MM,NextSecond}});
                NextSecond ->
                    {Date,{HH,MM,NextSecond}}
            end
    end,
    {ok, ecrontab_time_util:datetime_to_timestamp(DateTime)}.

%% ====================================================================
%% next_time_only_one end
%% ====================================================================

find_next_in_list(_,[]) ->
    false;
find_next_in_list(NowValue,[Value|_]) when NowValue<Value ->
    Value;
find_next_in_list(NowValue,[_|List]) ->
    find_next_in_list(NowValue,List).

is_passed_spec_field(_,#spec_field{type=?SPEC_FIELD_TYPE_ANY}) ->
    true;
is_passed_spec_field(Value,#spec_field{type=?SPEC_FIELD_TYPE_NUM,value=Value}) ->
    true;
is_passed_spec_field(Value,#spec_field{type=?SPEC_FIELD_TYPE_LIST,value=List}) ->
    ordsets:is_element(Value, List);
is_passed_spec_field(_,_) ->
    false.