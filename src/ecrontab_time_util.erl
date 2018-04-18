-module(ecrontab_time_util).
-include("ecrontab_parse.hrl").
-export([
    datetime_to_timestamp/1,
    timestamp_to_datetime/1,

    get_type_range/1,

    validate_year/1,
    validate_month/1,
    validate_week/1,
    validate_day/1,
    validate_hour/1,
    validate_minute/1,
    validate_second/1,

    prev_year/1, next_year/1,
    prev_month/1, next_month/1,
    prev_day/1, next_day/1,
    prev_hour/1, next_hour/1,
    prev_minute/1, next_minute/1,
    prev_second/1, next_second/1,

    validate_time/1, validate_time/3,

    get_datetime_year/1,
    get_datetime_month/1,
    get_datetime_day/1,
    get_datetime_week/1,
    get_datetime_hour/1,
    get_datetime_minute/1,
    get_datetime_second/1,

    get_datetime_by_type/2
]).

%% ====================================================================

%% @doc Translate UNIX timestamp to local datetime.
timestamp_to_datetime(Seconds) ->
    erlang:universaltime_to_localtime(erlang:posixtime_to_universaltime(Seconds)).

%% @doc Translate a local time date to UNIX timestamp
datetime_to_timestamp(undefined) -> undefined;
datetime_to_timestamp(DT) ->
    erlang:universaltime_to_posixtime(erlang:localtime_to_universaltime(DT)).

%% ====================================================================

get_type_range(?POS_YEAR) ->
    {error, year_range};
get_type_range(?POS_MONTH) ->
    {ok, {1, 12}};
get_type_range(?POS_DAY) ->
    {ok, {1, 31}};
get_type_range(?POS_WEEK) ->
    {ok, {1, 7}};
get_type_range(?POS_HOUR) ->
    {ok, {0, 23}};
get_type_range(?POS_MINUTE) ->
    {ok, {0, 59}};
get_type_range(?POS_SECOND) ->
    {ok, {0, 59}}.

%% ====================================================================

validate_year(Y) when is_integer(Y) andalso Y >= 0 ->
    {ok, Y};
validate_year(Binary) when is_binary(Binary) ->
    validate_year(binary_to_integer(Binary));
validate_year(_) ->
    {error, invalid_value}.

validate_month(M) when is_integer(M) andalso M >= 1 andalso M =< 12 ->
    {ok, M};
validate_month(List) when is_list(List) ->
    validate_month(list_to_binary(List));

validate_month(<<"January">>) -> {ok, 1};
validate_month(<<"February">>) -> {ok, 2};
validate_month(<<"March">>) -> {ok, 3};
validate_month(<<"April">>) -> {ok, 4};
validate_month(<<"May">>) -> {ok, 5};
validate_month(<<"June">>) -> {ok, 6};
validate_month(<<"July">>) -> {ok, 7};
validate_month(<<"August">>) -> {ok, 8};
validate_month(<<"September">>) -> {ok, 9};
validate_month(<<"October">>) -> {ok, 10};
validate_month(<<"November">>) -> {ok, 11};
validate_month(<<"December">>) -> {ok, 12};

validate_month(<<"january">>) -> {ok, 1};
validate_month(<<"february">>) -> {ok, 2};
validate_month(<<"march">>) -> {ok, 3};
validate_month(<<"april">>) -> {ok, 4};
validate_month(<<"may">>) -> {ok, 5};
validate_month(<<"june">>) -> {ok, 6};
validate_month(<<"july">>) -> {ok, 7};
validate_month(<<"august">>) -> {ok, 8};
validate_month(<<"september">>) -> {ok, 9};
validate_month(<<"october">>) -> {ok, 10};
validate_month(<<"november">>) -> {ok, 11};
validate_month(<<"december">>) -> {ok, 12};

validate_month(<<"Jan">>) -> {ok, 1};
validate_month(<<"Feb">>) -> {ok, 2};
validate_month(<<"Mar">>) -> {ok, 3};
validate_month(<<"Apr">>) -> {ok, 4};
validate_month(<<"Jun">>) -> {ok, 6};
validate_month(<<"Jul">>) -> {ok, 7};
validate_month(<<"Aug">>) -> {ok, 8};
validate_month(<<"Sep">>) -> {ok, 9};
validate_month(<<"Oct">>) -> {ok, 10};
validate_month(<<"Nov">>) -> {ok, 11};
validate_month(<<"Dec">>) -> {ok, 12};

validate_month(<<"jan">>) -> {ok, 1};
validate_month(<<"feb">>) -> {ok, 2};
validate_month(<<"mar">>) -> {ok, 3};
validate_month(<<"apr">>) -> {ok, 4};
validate_month(<<"jun">>) -> {ok, 6};
validate_month(<<"jul">>) -> {ok, 7};
validate_month(<<"aug">>) -> {ok, 8};
validate_month(<<"sep">>) -> {ok, 9};
validate_month(<<"oct">>) -> {ok, 10};
validate_month(<<"nov">>) -> {ok, 11};
validate_month(<<"dec">>) -> {ok, 12};

validate_month(Binary) when is_binary(Binary) ->
    validate_month(binary_to_integer(Binary));
validate_month(Atom) when is_atom(Atom) ->
    validate_month(atom_to_binary(Atom, utf8));
validate_month(_) ->
    {error, invalid_value}.


validate_week(0) ->
    {ok, 7};
validate_week(W) when is_integer(W) andalso W >= 1 andalso W =< 7 ->
    {ok, W};
validate_week(List) when is_list(List) ->
    validate_week(list_to_binary(List));

validate_week(<<"Mon">>) -> {ok, 1};
validate_week(<<"Tue">>) -> {ok, 2};
validate_week(<<"Wed">>) -> {ok, 3};
validate_week(<<"Thu">>) -> {ok, 4};
validate_week(<<"Fri">>) -> {ok, 5};
validate_week(<<"Sat">>) -> {ok, 6};
validate_week(<<"Sun">>) -> {ok, 7};

validate_week(<<"mon">>) -> {ok, 1};
validate_week(<<"tue">>) -> {ok, 2};
validate_week(<<"wed">>) -> {ok, 3};
validate_week(<<"thu">>) -> {ok, 4};
validate_week(<<"fri">>) -> {ok, 5};
validate_week(<<"sat">>) -> {ok, 6};
validate_week(<<"sun">>) -> {ok, 7};

validate_week(<<"Monday">>) -> {ok, 1};
validate_week(<<"Tuesday">>) -> {ok, 2};
validate_week(<<"Wednesday">>) -> {ok, 3};
validate_week(<<"Thursday">>) -> {ok, 4};
validate_week(<<"Friday">>) -> {ok, 5};
validate_week(<<"Saturday">>) -> {ok, 6};
validate_week(<<"Sunday">>) -> {ok, 7};

validate_week(<<"monday">>) -> {ok, 1};
validate_week(<<"tuesday">>) -> {ok, 2};
validate_week(<<"wednesday">>) -> {ok, 3};
validate_week(<<"thursday">>) -> {ok, 4};
validate_week(<<"friday">>) -> {ok, 5};
validate_week(<<"saturday">>) -> {ok, 6};
validate_week(<<"sunday">>) -> {ok, 7};

validate_week(<<"0">>) ->
    {ok, 7};
validate_week(Binary) when is_binary(Binary) ->
    validate_week(binary_to_integer(Binary));
validate_week(Atom) when is_atom(Atom) ->
    validate_week(atom_to_binary(Atom, utf8));
validate_week(_) ->
    {error, invalid_value}.

validate_day(D) when is_integer(D) andalso D >= 1 andalso D =< 31 ->
    {ok, D};
validate_day(Binary) when is_binary(Binary) ->
    validate_day(binary_to_integer(Binary));
validate_day(_) ->
    {error, invalid_value}.

validate_hour(H) when is_integer(H) andalso H >= 0 andalso H =< 23 ->
    {ok, H};
validate_hour(Binary) when is_binary(Binary) ->
    validate_hour(binary_to_integer(Binary));
validate_hour(_) ->
    {error, invalid_value}.

validate_minute(M) when is_integer(M) andalso M >= 0 andalso M =< 59 ->
    {ok, M};
validate_minute(Binary) when is_binary(Binary) ->
    validate_minute(binary_to_integer(Binary));
validate_minute(_) ->
    {error, invalid_value}.

validate_second(S) when is_integer(S) andalso S >= 0 andalso S =< 59 ->
    {ok, S};
validate_second(Binary) when is_binary(Binary) ->
    validate_second(binary_to_integer(Binary));
validate_second(_) ->
    {error, invalid_value}.

%% ====================================================================

%% @doc Return the date one year earlier.
%% ``` e.g. {{1995, 2, 28}, {0, 0, 0}} => {{1994, 2, 28}, {0, 0, 0}} '''
%% @end
-spec prev_year(calendar:datetime()) -> calendar:datetime().
prev_year({{Y, 2, 29}, T}) ->
    {{Y - 1, 3, 1}, T};
prev_year({{Y, M, D}, T}) ->
    {{Y - 1, M, D}, T}.

%% @doc Return the date one month earlier. Gives unpredictable results if the
%% day doesn't exist in the next month. (eg. feb 30 will become feb 28).
%% ``` e.g. {{1995, 2, 28}, {0, 0, 0}} => {{1995,1,28},{0,0,0}} '''
%% @end
-spec prev_month(calendar:datetime()) -> calendar:datetime().
prev_month({{Y, 1, D}, T}) -> {{Y - 1, 12, D}, T};
prev_month({{Y, M, D}, T}) -> move_day_if_undefined({{Y, M - 1, D}, T}, fun prev_day/1).

%% @doc Return the date one day earlier.
%% @end
-spec prev_day(calendar:datetime()) -> calendar:datetime().
prev_day({{_, _, 1}, _} = Date) ->
    {{Y1, M1, _}, T1} = prev_month(Date),
    {{Y1, M1, calendar:last_day_of_the_month(Y1, M1)}, T1};
prev_day({{Y, M, D}, T}) ->
    {{Y, M, D - 1}, T};
prev_day({_, _, _} = Date) ->
    prev_day({Date, {0, 0, 0}}).

%% @doc Return the date one hour earlier.
%% @end
-spec prev_hour(calendar:datetime()) -> calendar:datetime().
prev_hour({_, {0, _, _}} = Date) ->
    {YMD, {_, I, S}} = prev_day(Date),
    {YMD, {23, I, S}};
prev_hour({YMD, {H, I, S}}) ->
    {YMD, {H - 1, I, S}}.

%% @doc Return the date one minute earlier.
%% @end
-spec prev_minute(calendar:datetime()) -> calendar:datetime().
prev_minute({_, {_, 0, _}} = Date) ->
    {YMD, {H, _, S}} = prev_hour(Date),
    {YMD, {H, 59, S}};
prev_minute({YMD, {H, I, S}}) ->
    {YMD, {H, I - 1, S}}.

%% @doc Return the date one second earlier.
%% @end
-spec prev_second(calendar:datetime()) -> calendar:datetime().
prev_second({_, {_, _, 0}} = Date) ->
    {YMD, {H, I, _}} = prev_minute(Date),
    {YMD, {H, I, 59}};
prev_second({YMD, {H, I, S}}) ->
    {YMD, {H, I, S - 1}}.

%% ====================================================================

%% @doc Return the date one year later.
next_year({{Y, 2, 29}, T}) ->
    {{Y + 1, 3, 1}, T};
next_year({{Y, M, D}, T}) ->
    {{Y + 1, M, D}, T}.

%% @doc Return the date one month later.
next_month({{Y, 12, D}, T}) -> {{Y + 1, 1, D}, T};
next_month({{Y, M, D}, T}) -> move_day_if_undefined({{Y, M + 1, D}, T}, fun prev_day/1).

%% @doc Return the date one day later.
next_day({{Y, M, D}, T} = Date) ->
    case calendar:last_day_of_the_month(Y, M) of
        D ->
            {{Y1, M1, _}, T1} = next_month(Date),
            {{Y1, M1, 1}, T1};
        _ ->
            {{Y, M, D + 1}, T}
    end;
next_day({_, _, _} = Date) ->
    next_day({Date, {0, 0, 0}}).

%% @doc Return the date one hour later.
next_hour({_, {23, _, _}} = Date) ->
    {YMD, {_, I, S}} = next_day(Date),
    {YMD, {0, I, S}};
next_hour({YMD, {H, I, S}}) ->
    {YMD, {H + 1, I, S}}.

%% @doc Return the date one minute later.
next_minute({_, {_, 59, _}} = Date) ->
    {YMD, {H, _, S}} = next_hour(Date),
    {YMD, {H, 0, S}};
next_minute({YMD, {H, I, S}}) ->
    {YMD, {H, I + 1, S}}.

%% @doc Return the date one second later.
next_second({_, {_, _, 59}} = Date) ->
    {YMD, {H, I, _}} = next_minute(Date),
    {YMD, {H, I, 0}};
next_second({YMD, {H, I, S}}) ->
    {YMD, {H, I, S + 1}}.

%% ====================================================================

validate_time({HH, MM, SS}) ->
    validate_time(HH, MM, SS).
validate_time(HH, MM, SS) ->
    HH >= 0 andalso HH =< 23 andalso
        MM >= 0 andalso MM =< 60 andalso
        SS >= 0 andalso SS =< 60.

%% ====================================================================

get_datetime_year({{Value, _, _}, _}) ->
    Value.
get_datetime_month({{_, Value, _}, _}) ->
    Value.
get_datetime_day({{_, _, Value}, _}) ->
    Value.
get_datetime_week({Date, _}) ->
    calendar:day_of_the_week(Date).
get_datetime_hour({_, {Value, _, _}}) ->
    Value.
get_datetime_minute({_, {_, Value, _}}) ->
    Value.
get_datetime_second({_, {_, _, Value}}) ->
    Value.

%% ====================================================================
get_datetime_by_type(?POS_YEAR, {{Value, _, _}, _}) ->
    Value;
get_datetime_by_type(?POS_MONTH, {{_, Value, _}, _}) ->
    Value;
get_datetime_by_type(?POS_DAY, {{_, _, Value}, _}) ->
    Value;
get_datetime_by_type(?POS_WEEK, {Date, _}) ->
    calendar:day_of_the_week(Date);
get_datetime_by_type(?POS_HOUR, {_, {Value, _, _}}) ->
    Value;
get_datetime_by_type(?POS_MINUTE, {_, {_, Value, _}}) ->
    Value;
get_datetime_by_type(?POS_SECOND, {_, {_, _, Value}}) ->
    Value.

move_day_if_undefined(Date, Fun) ->
    case undefined_if_invalid_date(Date) of
        undefined -> move_day_if_undefined(Fun(Date), Fun);
        _ -> Date
    end.

%% @doc Return 'undefined' if a given date is invalid
undefined_if_invalid_date({{Y, M, D}, {H, I, S}} = Date) when
    is_integer(Y), is_integer(M), is_integer(D),
    is_integer(H), is_integer(I), is_integer(S),
    H >= 0, H =< 23, I >= 0, I =< 59, S >= 0, S =< 59,
    M >= 1, M =< 12, D >= 1, Y >= -4713, Y =< 9999 ->
    MaxDays = calendar:last_day_of_the_month(Y, M),
    case D =< MaxDays of
        true -> Date;
        false -> undefined
    end;
undefined_if_invalid_date(_) ->
    undefined.
