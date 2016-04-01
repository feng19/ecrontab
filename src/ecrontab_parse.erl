-module(ecrontab_parse).
-include("ecrontab_parse.hrl").
-export([
    parse_spec/1, parse_spec/2,
    parse_spec/5, parse_spec/7, parse_spec/8,
    get_spec_type/1
]).

-record(stat_spec_type, {
    type_any_count = 0,
    type_num_count = 0,
    type_list_count = 0
}).

-define(DEFAULT_OPTIONS, [{filter_over_time,now}]).

%% ====================================================================
%% parse_spec
%% ====================================================================
parse_spec(Spec) ->
    parse_spec(Spec, ?DEFAULT_OPTIONS).
parse_spec([Month, Day, Week, Hour, Minute], Options) ->
    parse_spec('*', Month, Day, Week, Hour, Minute, 0, Options);
parse_spec({Month, Day, Week, Hour, Minute}, Options) ->
    parse_spec('*', Month, Day, Week, Hour, Minute, 0, Options);
parse_spec([Year, Month, Day, Week, Hour, Minute, Second], Options) ->
    parse_spec(Year, Month, Day, Week, Hour, Minute, Second, Options);
parse_spec({Year, Month, Day, Week, Hour, Minute, Second}, Options) ->
    parse_spec(Year, Month, Day, Week, Hour, Minute, Second, Options).

parse_spec(Month, Day, Week, Hour, Minute) ->
    parse_spec('*', Month, Day, Week, Hour, Minute, 0).
parse_spec(Year, Month, Day, Week, Hour, Minute, Second) ->
    parse_spec(Year, Month, Day, Week, Hour, Minute, Second, ?DEFAULT_OPTIONS).

-spec parse_spec(Year :: any(), Month :: any(), Day :: any(), Week :: any(),
    Hour :: any(), Minute :: any(), Second :: any()) -> {ok, spec()}|{error, any()}.
parse_spec(Year, Month, Day, Week, Hour, Minute, Second, Options) ->
    case validate_spec([Year, Month, Day, Week, Hour, Minute, Second]) of
        {ok, Spec0} ->
            Spec = get_spec_type(Spec0),
            case proplists:get_value(filter_over_time, Options) of
                undefined ->
                    {ok, Spec};
                now ->
                    NowDatetime = erlang:localtime(),
                    filter_over_time(Spec, NowDatetime);
                NowDatetime ->
                    filter_over_time(Spec, NowDatetime)
            end;
        Err ->
            Err
    end.

validate_spec(Spec) ->
    validate_spec(Spec, ?POS_YEAR, []).
validate_spec([Value|T], Pos, Spec) ->
    case parse_spec_field(Value, Pos) of
        {ok, SpecField} ->
%%            NewSpec = erlang:setelement(Pos, Spec, SpecField),
            validate_spec(T, Pos+1, [SpecField|Spec]);
        {error, Err} ->
            {error, {pos2name(Pos), Err}}
    end;
validate_spec([], _, [Second, Minute, Hour, Week, Day, Month, Year]) ->
    {ok, #spec{year = Year, month = Month, day = Day, week = Week, hour = Hour, minute = Minute, second = Second}}.

pos2name(?POS_YEAR) ->      year;
pos2name(?POS_MONTH) ->     month;
pos2name(?POS_DAY) ->       day;
pos2name(?POS_WEEK) ->      week;
pos2name(?POS_HOUR) ->      hour;
pos2name(?POS_MINUTE) ->    minute;
pos2name(?POS_SECOND) ->    second.

%% ====================================================================
%% parse_spec_field
%% ====================================================================
parse_spec_field(<<>>, _) ->
    {error, empty_binary};
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
            case binary:match(Single,[<<"/">>,<<"-">>]) of
                nomatch ->
                    parse_spec_field_other(Single, Type);
                _ ->
                    parse_interval(Single, Type)
            end;
        List ->
            parse_list(List, Type)
    end;
parse_spec_field(Value0, Type) ->
    parse_spec_field_other(Value0, Type).

parse_spec_field_other(Value0, Type) ->
    case validate_value(Type, Value0) of
        {ok,Value} ->
            {ok, #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = Value}};
        Err ->
            Err
    end.

%% ====================================================================
%% parse_list
%% ====================================================================
parse_list([], _Type) ->
    {error, empty_list};
parse_list(List, Type) ->
    parse_list(List, Type, []).

parse_list([H|T], Type, Acc) ->
    case parse_spec_field(H, Type) of
        {ok,#spec_field{type = ?SPEC_FIELD_TYPE_ANY}} ->
            {error, list_any};
        {ok,#spec_field{type = ?SPEC_FIELD_TYPE_INTERVAL}} ->
            {error, list_interval};
        {ok,#spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = Value}} ->
            parse_list(T, Type, [Value|Acc]);
        {ok,#spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = Value}} ->
            parse_list(T, Type, Value ++ Acc);
        Err ->
            Err
    end;
parse_list([], _Type, Acc) ->
    List = ordsets:from_list(Acc),
    list_single(List).

list_single([Value]) ->
    {ok, #spec_field{type = ?SPEC_FIELD_TYPE_NUM, value = Value}};
list_single(List) ->
    {ok, #spec_field{type = ?SPEC_FIELD_TYPE_LIST, value = List}}.

%% ====================================================================
%% parse interval and range binary:
%%  <<"2-5/2">>, <<"2-5">>, <<"23-7/2">>, <<"*/2">>, <<"/2">>
%% ====================================================================
parse_interval(Bin, Type) ->
    case binary:split(Bin, <<"/">>, [global]) of
        [<<>>, IntervalBin] ->
            parse_interval_any(Type, IntervalBin);
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
        [RangeBin] ->
            parse_interval_do(Type, RangeBin, 1)
    end.
parse_interval_do(Type, RangeBin, Step) ->
    case binary:split(RangeBin, <<"-">>) of
        [<<>>, <<>>] ->
            {error, invalid_range};
        [<<>>, Last] ->
            case ecrontab_time_util:get_type_range(Type) of
                {ok, {Min,_}} ->
                    parse_interval_do(Type, Min, Last, Step);
                _ ->
                    {error, invalid_range}
            end;
        [First,<<>>] ->
            case ecrontab_time_util:get_type_range(Type) of
                {ok, {_,Max}} ->
                    parse_interval_do(Type, First, Max, Step);
                _ ->
                    {error, invalid_range}
            end;
        [Value, Value] -> % First==Last
            {error, same_range};
        [First, Last] ->
            parse_interval_do(Type, First, Last, Step);
        [First] ->
            case ecrontab_time_util:get_type_range(Type) of
                {ok, {_,Max}} ->
                    parse_interval_do(Type, First, Max, Step);
                _ ->
                    {error, invalid_range}
            end
    end.
parse_interval_do(Type, First0, Last0, Step) ->
    case validate_value(Type, First0) of
        {ok, First} ->
            case validate_value(Type, Last0) of
                {ok, Last} when First=<Last ->
                    List = get_list_by_range(First, Last, Step),
                    list_single(lists:usort(List));
                {ok, Last} -> % First > Last
                    case get_list_by_range(Type, First, Last, Step) of
                        {ok, List} ->
                            list_single(lists:usort(List));
                        Err ->
                            Err
                    end;
                Err ->
                    Err
            end;
        Err ->
            Err
    end.

parse_interval_one(Type) ->
    case Type of
        ?POS_YEAR ->
            {ok, #spec_field{type = ?SPEC_FIELD_TYPE_INTERVAL,
                             value = 1}};
        _ ->
            {ok, {Min,Max}} = ecrontab_time_util:get_type_range(Type),
            {ok, #spec_field{type = ?SPEC_FIELD_TYPE_LIST,
                             value = lists:seq(Min,Max)}}
    end.

parse_interval_any(?POS_YEAR, IntervalBin) ->
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
            list_single(lists:usort(List));
        Err ->
            Err
    end.

check_interval_bin(<<>>) ->
    {error, invalid_step};
check_interval_bin(<<"*">>) ->
    {error, invalid_step};
check_interval_bin(Bin) ->
    Int = binary_to_integer(Bin),
    if
        Int =< 0 ->
            {error, neg_integer};
        true ->
            {ok, Int}
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
            {ok, get_list_by_range_do(Min, Max, First, Last, Step)};
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
validate_value(?POS_YEAR, Value) ->
    ecrontab_time_util:validate_year(Value);
validate_value(?POS_MONTH, Value) ->
    ecrontab_time_util:validate_month(Value);
validate_value(?POS_DAY, Value) ->
    ecrontab_time_util:validate_day(Value);
validate_value(?POS_HOUR, Value) ->
    ecrontab_time_util:validate_hour(Value);
validate_value(?POS_WEEK, Value) ->
    ecrontab_time_util:validate_week(Value);
validate_value(?POS_MINUTE, Value) ->
    ecrontab_time_util:validate_minute(Value);
validate_value(?POS_SECOND, Value) ->
    ecrontab_time_util:validate_second(Value).


filter_over_time(Spec, NowDatetime) ->
    case Spec#spec.type of
        ?SPEC_TYPE_SECONDS ->
            NowSeconds = calendar:datetime_to_gregorian_seconds(NowDatetime),
            if
                NowSeconds > Spec#spec.value ->
                    {error, time_over};
                true ->
                    {ok, Spec}
            end;
        ?SPEC_TYPE_ONLY_ONE when Spec#spec.value==?POS_YEAR ->
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
                    {error, time_over};
                true ->
                    {ok, Spec}
            end;
        ?SPEC_FIELD_TYPE_LIST ->
            NewValue = filter_less_then(NowYear,SpecField#spec_field.value),
            {ok, NewSpecField} = list_single(NewValue),
            NewSpec = get_spec_type(Spec#spec{year = NewSpecField}),
            {ok, NewSpec};
        _ ->
            {ok, Spec}
    end.

filter_less_then(Value,[V|Ordsets]) when Value > V ->
    filter_less_then(Value,Ordsets);
filter_less_then(_,Ordsets) ->
    Ordsets.

get_spec_type(#spec{year = Year, month = Month, day = Day, week = Week,
                    hour = Hour, minute = Minute, second = Second} = Spec) ->
    {SpecType, SpecTypeValue} =
    case Year of
        #spec_field{type = ?SPEC_FIELD_TYPE_INTERVAL, value = Interval} ->
            {?SPEC_TYPE_INTERVAL_YEAR, Interval};
        _ ->
            List = [Year, Month, Day, Week, Hour, Minute, Second],
            case stat_spec_type(List) of
                #stat_spec_type{type_any_count = 7} ->
                    {?SPEC_TYPE_EVERY_SECOND, none};
                #stat_spec_type{type_any_count = 1,type_num_count = 6} ->
                    case Spec#spec.week#spec_field.type of
                        ?SPEC_FIELD_TYPE_ANY ->
                            Datatime = {
                                {Year#spec_field.value, Month#spec_field.value, Day#spec_field.value},
                                {Hour#spec_field.value, Minute#spec_field.value, Second#spec_field.value}
                            },
                            Seconds = calendar:datetime_to_gregorian_seconds(Datatime),
                            {?SPEC_TYPE_SECONDS, Seconds};
                        _ ->
                            {?SPEC_TYPE_NORMAL, none}
                    end;
                #stat_spec_type{type_any_count = 6} ->
                    Pos = get_only_one_pos(List),
                    {?SPEC_TYPE_ONLY_ONE, Pos};
                _ ->
                    {?SPEC_TYPE_NORMAL, none}
            end
    end,
    Spec#spec{type = SpecType, value = SpecTypeValue}.

stat_spec_type(List) ->
    stat_spec_type(List, #stat_spec_type{}).
stat_spec_type([#spec_field{type=?SPEC_FIELD_TYPE_ANY}|List],Stat) ->
    stat_spec_type(List, Stat#stat_spec_type{type_any_count=Stat#stat_spec_type.type_any_count+1});
stat_spec_type([#spec_field{type=?SPEC_FIELD_TYPE_NUM}|List],Stat) ->
    stat_spec_type(List, Stat#stat_spec_type{type_num_count=Stat#stat_spec_type.type_num_count+1});
stat_spec_type([#spec_field{type=?SPEC_FIELD_TYPE_LIST}|List],Stat) ->
    stat_spec_type(List, Stat#stat_spec_type{type_list_count=Stat#stat_spec_type.type_list_count+1});
stat_spec_type([], Stat) ->
    Stat.

get_only_one_pos(List) ->
    get_only_one_pos(List, ?POS_YEAR).
get_only_one_pos([#spec_field{type=?SPEC_FIELD_TYPE_ANY}|List],Pos) ->
    get_only_one_pos(List,Pos+1);
get_only_one_pos([_SpecField|_],Pos) ->
    Pos.