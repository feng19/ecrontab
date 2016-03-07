-module(ecrontab_parse).
-include("ecrontab.hrl").
-export([
    parse_spec/1, parse_spec/5, parse_spec/7,
    get_spec_type/1
]).

-record(stat_spec_type, {
    type_any_count = 0,
    type_num_count = 0,
    type_list_count = 0
}).

%% ====================================================================
%% parse_spec
%% ====================================================================
parse_spec([Month, Day, Week, Hour, Minute]) ->
    parse_spec('*', Month, Day, Week, Hour, Minute, 0);
parse_spec({Month, Day, Week, Hour, Minute}) ->
    parse_spec('*', Month, Day, Week, Hour, Minute, 0);
parse_spec([Year, Month, Day, Week, Hour, Minute, Second]) ->
    parse_spec(Year, Month, Day, Week, Hour, Minute, Second);
parse_spec({Year, Month, Day, Week, Hour, Minute, Second}) ->
    parse_spec(Year, Month, Day, Week, Hour, Minute, Second).

parse_spec(Month, Day, Week, Hour, Minute) ->
    parse_spec('*', Month, Day, Week, Hour, Minute, 0).
-spec parse_spec(Year :: any(), Month :: any(), Day :: any(), Week :: any(),
    Hour :: any(), Minute :: any(), Second :: any()) -> {ok, spec()}|{error, any()}.
parse_spec(Year, Month, Day, Week, Hour, Minute, Second) ->
    List = [{year, Year}, {month, Month}, {day, Day}, {week, Week},
            {hour, Hour}, {minute, Minute},{second, Second}],
    case validate_spec(List) of
        {ok, NewList} ->
            {SpecType, SpecTypeValue} = get_spec_type(NewList),
            Spec0 = list_to_tuple([spec,SpecType,SpecTypeValue|NewList]),
            NowDatetime = erlang:localtime(),
            NowTimestamp = ecrontab_time_util:datetime_to_timestamp(NowDatetime),
            filter_over_time(Spec0, NowDatetime, NowTimestamp);
        Err ->
            Err
    end.

validate_spec(Spec) ->
    validate_spec(Spec, []).
validate_spec([{Type,Value}|T], Acc) ->
    case parse_spec_field(Value, Type) of
        {ok, SpecField} ->
            validate_spec(T, [SpecField|Acc]);
        {error, Err} ->
            {error, {Type, Err}}
    end;
validate_spec([], Acc) ->
    {ok, lists:reverse(Acc)}.

%% ====================================================================
%% parse_spec_field
%% ====================================================================
parse_spec_field('*', _Type) ->
    {ok,#spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}};
parse_spec_field(<<"*">>, _Type) ->
    {ok,#spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}};
parse_spec_field("*", _Type) ->
    {ok,#spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}};
parse_spec_field(List, Type) when is_list(List) ->
    parse_list(List, Type);
parse_spec_field(Value, Type) when is_binary(Value) ->
    case binary:split(Value, <<",">>, [global]) of
        [Single] ->
            parse_interval(Single, Type);
        [] ->
            {error, binary_field};
        List ->
            parse_list(List, Type)
    end;
parse_spec_field(Value0, Type) ->
    case validate_value(Type, Value0) of
        {ok,Value} ->
            {ok, #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = Value}};
        Err ->
            Err
    end.

%% parse_list
parse_list([], _Type) ->
    {error, empty_list};
parse_list(List, Type) ->
    parse_list(lists:usort(List), Type, []).

parse_list([H|T], Type, Acc) ->
    case parse_spec_field(H, Type) of
        {ok,#spec_field{type = ?SPEC_FIELD_ANY}} ->
            {error, list_any};
        {ok,Value} ->
            parse_list(T, Type, [Value|Acc]);
        Err ->
            Err
    end;
parse_list([], Type, Acc) ->
    case filter_list_overlap(Acc) of
        {ok, List} ->
            list_single(Type, List);
        Err ->
            Err
    end.

list_single(_, [Value]) ->
    {ok, #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = Value}};
list_single(year, List) ->
    {ok, #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = List}};
list_single(month, [1,2,3,4,5,6,7,8,9,10,11,12]) ->
    {ok, #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}};
list_single(day, [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31]) ->
    {ok, #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}};
list_single(week, [1,2,3,4,5,6,7]) ->
    {ok, #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}};
list_single(hour, [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
    20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,
    40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59]) ->
    {ok, #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}};
list_single(minute, [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
    20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,
    40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59]) ->
    {ok, #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}};
list_single(second, [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
    20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,
    40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59]) ->
    {ok, #spec_field{type = ?SPEC_FIELD_TYPE_ANY, value = ?SPEC_FIELD_ANY}};
list_single(_, List) ->
    {ok, #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = List}}.

filter_list_overlap(List) ->
    case filter_list_overlap_do(List,[]) of
        {ok, NewList} ->
            {ok, lists:usort(NewList)};
        Err ->
            Err
    end.

filter_list_overlap_do([],ListAcc) ->
    {ok, ListAcc};
filter_list_overlap_do([SpecField|List],Acc) ->
    case SpecField#spec_field.type of
        ?SPEC_FIELD_TYPE_NUM ->
            NewAcc = [SpecField#spec_field.value|Acc],
            filter_list_overlap_do(List,NewAcc);
        ?SPEC_FIELD_TYPE_LIST ->
            case filter_list_overlap(SpecField#spec_field.value) of
                {ok, NewList} ->
                    NewAcc = NewList++Acc,
                    filter_list_overlap_do(List,NewAcc);
                Err ->
                    Err
            end;
        _ ->
            {error, list_other}
    end.

parse_interval_one(Type) ->
    case Type of
        year ->
            {ok, #spec_field{type = ?SPEC_FIELD_TYPE_INTERVAL,
                             value = 1}};
        _ ->
            {ok, {Min,Max}} = ecrontab_time_util:get_type_range(Type),
            {ok, #spec_field{type = ?SPEC_FIELD_TYPE_LIST,
                             value = lists:seq(Min,Max)}}
    end.

parse_interval_any(year, IntervalBin) ->
    case check_interval_bin(IntervalBin) of
        {ok, Interval} ->
            {ok, #spec_field{type = ?SPEC_FIELD_TYPE_INTERVAL,
                value = Interval}};
        Err ->
            Err
    end;
parse_interval_any(Type, IntervalBin) ->
    case check_interval_bin(IntervalBin) of
        {ok, Interval} ->
            {ok, {Min,Max}} = ecrontab_time_util:get_type_range(Type),
            List = get_list_by_range(Min, Max, Interval),
            list_single(Type, List);
        Err ->
            Err
    end.

check_interval_bin(Bin) ->
    Int = binary_to_integer(Bin),
    if
        Int =< 0 ->
            {error, neg_integer};
        true ->
            {ok, Int}
    end.

%% parse the range binary: 
%%     <<"2-5/2">>, <<"2-5">>, <<"23-7/2">>, <<"*/2">>, <<"/2">>
parse_interval(Bin, Type) ->
    case binary:split(Bin, <<"/">>, [global]) of
        [<<>>, IntervalBin] ->
            parse_interval_any(Type, IntervalBin);
        [RangeBin] ->
            parse_interval_do(Type, RangeBin, 1);
        [<<"*">>, <<"1">>] ->
            parse_interval_one(Type);
        [<<"*">>, IntervalBin] ->
            parse_interval_any(Type, IntervalBin);
        [RangeBin, <<"1">>] ->
            parse_interval_do(Type, RangeBin, 1);
        [RangeBin, IntervalBin] ->
            case check_interval_bin(IntervalBin) of
                {ok, Interval} ->
                    parse_interval_do(Type, RangeBin, Interval);
                Err ->
                    Err
            end;
        [] ->
            {error, range}
    end.
parse_interval_do(Type, RangeBin, Step) ->
    case binary:split(RangeBin, <<"-">>) of
        [Value0, Value0] -> % First==Last
            {error, error_interval};
        [First0, Last0] ->
            parse_interval_do(Type, First0, Last0, Step);
        _ ->
            {error, interval}
    end.
parse_interval_do(Type, First0, Last0, Step) ->
    case validate_value(Type, First0) of
        {ok, First} ->
            case validate_value(Type, Last0) of
                {ok, Last} when First=<Last ->
                    List = get_list_by_range(First, Last, Step),
                    list_single(Type, List);
                {ok, Last} -> % First > Last
                    List = get_list_by_range(Type, First, Last, Step),
                    list_single(Type, List);
                Err ->
                    Err
            end;
        Err ->
            Err
    end.

%% First =< Last
get_list_by_range(First, Last, Step) ->
    NextFirst = First+Step,
    if
        NextFirst > Last ->
            [First];
        true ->
            [First | get_list_by_range(NextFirst,Last,Step)]
    end.

%% First > Last
get_list_by_range(Type, First, Last, Step) ->
    case ecrontab_time_util:get_type_range(Type) of
        {ok, {Min,Max}} ->
            get_list_by_range_do(Min, Max, First, Last, Step);
        Err ->
            Err
    end.
get_list_by_range_do(Min, Max, First, Last, Step) ->
    NextFirst = First+Step,
    if
        NextFirst > Max ->
            [First | get_list_by_range_over_max(NextFirst,Min,Max,Last,Step)];
        true ->
            [First | get_list_by_range_do(Min, Max, NextFirst, Last, Step)]
    end.
get_list_by_range_over_max(NextFirst0,Min,Max,Last,Step) ->
    NextFirst =
    case Min of
        0 ->
            NextFirst0 - Max - 1;
        _ ->
            NextFirst0 - Max
    end,
    if
        NextFirst > Last ->
            [];
        NextFirst == Last ->
            [Last];
        true ->
            get_list_by_range(NextFirst,Last,Step)
    end.

%% validate_value
validate_value(_Type, '*') ->
    {error, any};
validate_value(_Type, "*") ->
    {error, any};
validate_value(_Type, <<"*">>) ->
    {error, any};
validate_value(year, Value) ->
    ecrontab_time_util:validate_year(Value);
validate_value(month, Value) ->
    ecrontab_time_util:validate_month(Value);
validate_value(day, Value) ->
    ecrontab_time_util:validate_day(Value);
validate_value(hour, Value) ->
    ecrontab_time_util:validate_hour(Value);
validate_value(week, Value) ->
    ecrontab_time_util:validate_week(Value);
validate_value(minute, Value) ->
    ecrontab_time_util:validate_minute(Value);
validate_value(second, Value) ->
    ecrontab_time_util:validate_second(Value);
validate_value(_Type, _Value) ->
    {error, type}.


filter_over_time(Spec, NowDatetime, NowTimestamp) ->
    case Spec#spec.type of
        ?SPEC_TYPE_TIMESTAMP ->
            if
                NowTimestamp > Spec#spec.value ->
                    {error, over};
                true ->
                    {ok, Spec}
            end;
        ?SPEC_TYPE_ONLY_ONE when Spec#spec.value==#spec.year ->
            filter_over_time_year(Spec, NowDatetime);
        ?SPEC_TYPE_NORMAL ->
            filter_over_time_year(Spec, NowDatetime);
        _ ->
            {ok, Spec}
    end.

filter_over_time_year(Spec, NowDatetime) ->
    SpecField = Spec#spec.year,
    NowYear = ecrontab_time_util:get_datetime_year(NowDatetime),
    case SpecField#spec_field.type of
        ?SPEC_FIELD_TYPE_NUM ->
            if
                NowYear > SpecField#spec_field.value ->
                    {error, over};
                true ->
                    {ok, Spec}
            end;
        ?SPEC_FIELD_TYPE_LIST ->
            NewValue = filter_less_then(NowYear,SpecField#spec_field.value),
            {ok, NewSpecField} = list_single(year,NewValue),
            {ok, Spec#spec{year = NewSpecField}};
        _ ->
            {ok, Spec}
    end.

filter_less_then(Value,[V|Ordsets]) when Value > V ->
    filter_less_then(Value,Ordsets);
filter_less_then(_,Ordsets) ->
    Ordsets.

get_spec_type(#spec{year = Year, month = Month, day = Day, week = Week,
                    hour = Hour, minute = Minute, second = Second}) ->
    get_spec_type([Year, Month, Day, Week, Hour, Minute, Second]);
get_spec_type(List) ->
    case hd(List) of
        #spec_field{type = ?SPEC_FIELD_TYPE_INTERVAL, value = Interval} ->
            {?SPEC_TYPE_INTERVAL_YEAR, Interval};
        _ ->
            case stat_spec_type(List) of
                #stat_spec_type{type_any_count = 7} ->
                    {?SPEC_TYPE_EVERY_SECOND, none};
                #stat_spec_type{type_any_count = 1,type_num_count = 6} ->
                    is_type_timestamp(List);
                #stat_spec_type{type_any_count = 6} ->
                    only_one_value(List);
                _ ->
                    {?SPEC_TYPE_NORMAL, none}
            end
    end.

stat_spec_type(List) ->
    lists:foldl(fun stat_spec_type_do/2, #stat_spec_type{}, List).

stat_spec_type_do(#spec_field{type=?SPEC_FIELD_TYPE_ANY},Stat) ->
    Stat#stat_spec_type{type_any_count=Stat#stat_spec_type.type_any_count+1};
stat_spec_type_do(#spec_field{type=?SPEC_FIELD_TYPE_NUM},Stat) ->
    Stat#stat_spec_type{type_num_count=Stat#stat_spec_type.type_num_count+1};
stat_spec_type_do(#spec_field{type=?SPEC_FIELD_TYPE_LIST},Stat) ->
    Stat#stat_spec_type{type_list_count=Stat#stat_spec_type.type_list_count+1}.

only_one_value([_SpecField, #spec_field{type=?SPEC_FIELD_TYPE_ANY},
                #spec_field{type=?SPEC_FIELD_TYPE_ANY}, #spec_field{type=?SPEC_FIELD_TYPE_ANY},
                #spec_field{type=?SPEC_FIELD_TYPE_ANY}, #spec_field{type=?SPEC_FIELD_TYPE_ANY},
                #spec_field{type=?SPEC_FIELD_TYPE_ANY}]) ->
    {?SPEC_TYPE_ONLY_ONE, #spec.year};
only_one_value([#spec_field{type=?SPEC_FIELD_TYPE_ANY}, _SpecField,
                #spec_field{type=?SPEC_FIELD_TYPE_ANY}, #spec_field{type=?SPEC_FIELD_TYPE_ANY},
                #spec_field{type=?SPEC_FIELD_TYPE_ANY}, #spec_field{type=?SPEC_FIELD_TYPE_ANY},
                #spec_field{type=?SPEC_FIELD_TYPE_ANY}]) ->
    {?SPEC_TYPE_ONLY_ONE, #spec.month};
only_one_value([#spec_field{type=?SPEC_FIELD_TYPE_ANY}, #spec_field{type=?SPEC_FIELD_TYPE_ANY},
                _SpecField, #spec_field{type=?SPEC_FIELD_TYPE_ANY},
                #spec_field{type=?SPEC_FIELD_TYPE_ANY}, #spec_field{type=?SPEC_FIELD_TYPE_ANY},
                #spec_field{type=?SPEC_FIELD_TYPE_ANY}]) ->
    {?SPEC_TYPE_ONLY_ONE, #spec.day};
only_one_value([#spec_field{type=?SPEC_FIELD_TYPE_ANY}, #spec_field{type=?SPEC_FIELD_TYPE_ANY},
                #spec_field{type=?SPEC_FIELD_TYPE_ANY}, _SpecField,
                #spec_field{type=?SPEC_FIELD_TYPE_ANY}, #spec_field{type=?SPEC_FIELD_TYPE_ANY},
                #spec_field{type=?SPEC_FIELD_TYPE_ANY}]) ->
    {?SPEC_TYPE_ONLY_ONE, #spec.week};
only_one_value([#spec_field{type=?SPEC_FIELD_TYPE_ANY}, #spec_field{type=?SPEC_FIELD_TYPE_ANY},
                #spec_field{type=?SPEC_FIELD_TYPE_ANY}, #spec_field{type=?SPEC_FIELD_TYPE_ANY},
                _SpecField, #spec_field{type=?SPEC_FIELD_TYPE_ANY},
                #spec_field{type=?SPEC_FIELD_TYPE_ANY}]) ->
    {?SPEC_TYPE_ONLY_ONE, #spec.hour};
only_one_value([#spec_field{type=?SPEC_FIELD_TYPE_ANY}, #spec_field{type=?SPEC_FIELD_TYPE_ANY},
                #spec_field{type=?SPEC_FIELD_TYPE_ANY}, #spec_field{type=?SPEC_FIELD_TYPE_ANY},
                #spec_field{type=?SPEC_FIELD_TYPE_ANY}, _SpecField,
                #spec_field{type=?SPEC_FIELD_TYPE_ANY}]) ->
    {?SPEC_TYPE_ONLY_ONE, #spec.minute};
only_one_value([#spec_field{type=?SPEC_FIELD_TYPE_ANY}, #spec_field{type=?SPEC_FIELD_TYPE_ANY},
                #spec_field{type=?SPEC_FIELD_TYPE_ANY}, #spec_field{type=?SPEC_FIELD_TYPE_ANY},
                #spec_field{type=?SPEC_FIELD_TYPE_ANY}, #spec_field{type=?SPEC_FIELD_TYPE_ANY},
                _SpecField]) ->
    {?SPEC_TYPE_ONLY_ONE, #spec.second}.

is_type_timestamp([#spec_field{value=Year}, #spec_field{value=Month},
             #spec_field{value=Day}, #spec_field{type=?SPEC_FIELD_TYPE_ANY}, 
             #spec_field{value=Hour}, #spec_field{value=Minute},
             #spec_field{value=Second}]) ->
    Timestamp = ecrontab_time_util:datetime_to_timestamp({{Year, Month, Day}, {Hour, Minute, Second}}),
    {?SPEC_TYPE_TIMESTAMP, Timestamp};
is_type_timestamp(_) ->
    {?SPEC_TYPE_NORMAL, none}.