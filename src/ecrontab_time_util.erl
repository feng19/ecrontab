-module(ecrontab_time_util).
-export([
    timestamp/0,
    
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
    
    next_year/1,
    next_month/1,
    next_day/1,
    next_hour/1,
    next_minute/1,
    next_second/1,
    
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

%% Constant value of 
%%     calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})
-define(SECS_1970, 62167219200).

%% @doc Calculate the current UNIX timestamp (seconds since Jan 1, 1970)
timestamp() ->
    Datetime = calendar:universal_time(),
    calendar:datetime_to_gregorian_seconds(Datetime) - ?SECS_1970.

%% ====================================================================

datetime_to_timestamp(Datetime0) ->
    Datetime = erlang:localtime_to_universaltime(Datetime0),
    calendar:datetime_to_gregorian_seconds(Datetime) - ?SECS_1970.

timestamp_to_datetime(Timestamp) ->
    Datetime = calendar:gregorian_seconds_to_datetime(
                   ?SECS_1970 + Timestamp),
    erlang:universaltime_to_localtime(Datetime).

%% ====================================================================

get_type_range(year) ->
    {error, year_range};
get_type_range(month) ->
    {ok, {1, 12}};
get_type_range(day) ->
    {ok, {1, 31}};
get_type_range(week) ->
    {ok, {1, 7}};
get_type_range(hour) ->
    {ok, {0, 23}};
get_type_range(minute) ->
    {ok, {0, 59}};
get_type_range(second) ->
    {ok, {0, 59}}.


%% ====================================================================

validate_year(Y) when is_integer(Y) andalso Y >= 0 ->
    {ok, Y};
validate_year(Binary) when is_binary(Binary) ->
    validate_year(binary_to_integer(Binary));
validate_year(_) ->
    {error, invalid_value}.

validate_month(M) when is_integer(M) andalso M >= 1 andalso M =<12 ->
    {ok, M};
validate_month(List) when is_list(List) ->
    validate_month(list_to_binary(List));

validate_month(<<"January">>) ->    {ok,1};
validate_month(<<"February">>) ->   {ok,2};
validate_month(<<"March">>) ->      {ok,3};
validate_month(<<"April">>) ->      {ok,4};
validate_month(<<"May">>) ->        {ok,5};
validate_month(<<"June">>) ->       {ok,6};
validate_month(<<"July">>) ->       {ok,7};
validate_month(<<"August">>) ->     {ok,8};
validate_month(<<"September">>) ->  {ok,9};
validate_month(<<"October">>) ->    {ok,10};
validate_month(<<"November">>) ->   {ok,11};
validate_month(<<"December">>) ->   {ok,12};

validate_month(<<"january">>) ->    {ok,1};
validate_month(<<"february">>) ->   {ok,2};
validate_month(<<"march">>) ->      {ok,3};
validate_month(<<"april">>) ->      {ok,4};
validate_month(<<"may">>) ->        {ok,5};
validate_month(<<"june">>) ->       {ok,6};
validate_month(<<"july">>) ->       {ok,7};
validate_month(<<"august">>) ->     {ok,8};
validate_month(<<"september">>) ->  {ok,9};
validate_month(<<"october">>) ->    {ok,10};
validate_month(<<"november">>) ->   {ok,11};
validate_month(<<"december">>) ->   {ok,12};

validate_month(<<"Jan">>) ->    {ok,1};
validate_month(<<"Feb">>) ->    {ok,2};
validate_month(<<"Mar">>) ->    {ok,3};
validate_month(<<"Apr">>) ->    {ok,4};
validate_month(<<"Jun">>) ->    {ok,6};
validate_month(<<"Jul">>) ->    {ok,7};
validate_month(<<"Aug">>) ->    {ok,8};
validate_month(<<"Sep">>) ->    {ok,9};
validate_month(<<"Oct">>) ->    {ok,10};
validate_month(<<"Nov">>) ->    {ok,11};
validate_month(<<"Dec">>) ->    {ok,12};

validate_month(<<"jan">>) ->    {ok,1};
validate_month(<<"feb">>) ->    {ok,2};
validate_month(<<"mar">>) ->    {ok,3};
validate_month(<<"apr">>) ->    {ok,4};
validate_month(<<"jun">>) ->    {ok,6};
validate_month(<<"jul">>) ->    {ok,7};
validate_month(<<"aug">>) ->    {ok,8};
validate_month(<<"sep">>) ->    {ok,9};
validate_month(<<"oct">>) ->    {ok,10};
validate_month(<<"nov">>) ->    {ok,11};
validate_month(<<"dec">>) ->    {ok,12};

validate_month(Binary) when is_binary(Binary) ->
    validate_month(binary_to_integer(Binary));
validate_month(Atom) when is_atom(Atom) ->
    List = atom_to_list(Atom),
    validate_month(list_to_binary(List));
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
validate_week(<<"Monday">>) ->      {ok, 1};
validate_week(<<"Tuesday">>) ->     {ok, 2};
validate_week(<<"Sednesday">>) ->   {ok, 3};
validate_week(<<"Thursday">>) ->    {ok, 4};
validate_week(<<"Friday">>) ->      {ok, 5};
validate_week(<<"Saturday">>) ->    {ok, 6};
validate_week(<<"Sunday">>) ->      {ok, 7};

validate_week(<<"monday">>) ->      {ok, 1};
validate_week(<<"tuesday">>) ->     {ok, 2};
validate_week(<<"wednesday">>) ->   {ok, 3};
validate_week(<<"thursday">>) ->    {ok, 4};
validate_week(<<"friday">>) ->      {ok, 5};
validate_week(<<"saturday">>) ->    {ok, 6};
validate_week(<<"sunday">>) ->      {ok, 7};

validate_week(<<"0">>) ->
    {ok, 7};
validate_week(Binary) when is_binary(Binary) ->
    validate_week(binary_to_integer(Binary));
validate_week(Atom) when is_atom(Atom) ->
    List = atom_to_list(Atom),
    validate_week(list_to_binary(List));
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

%% @doc Return the date one year later.
next_year({{Y,2,29},T})  ->
    {{Y+1,3,1}, T};
next_year({{Y,M,D},T}) ->
    {{Y+1,M,D}, T}.

%% @doc Return the date one month later.
next_month({{Y,12,D},T}) -> {{Y+1,1,D},T};
next_month({{Y,M,D},T}) -> {{Y,M+1,D}, T}.

%% @doc Return the date one day later.
next_day({{Y,M,D},T} = Date) ->
    case calendar:last_day_of_the_month(Y,M) of
        D -> 
            {{Y1,M1,_},T1} = next_month(Date),
            {{Y1,M1,1},T1};
        _ ->
            {{Y,M,D+1},T}
    end;
next_day({_,_,_} = Date) ->
    next_day({Date, {0,0,0}}).

%% @doc Return the date one hour later.
next_hour({_,{23,_,_}} = Date) ->
    {YMD,{_,I,S}} = next_day(Date),
    {YMD,{0,I,S}};
next_hour({YMD,{H,I,S}}) ->
    {YMD, {H+1,I,S}}.

%% @doc Return the date one minute later.
next_minute({_,{_,59,_}} = Date) ->
    {YMD,{H,_,S}} = next_hour(Date),
    {YMD,{H,0,S}};
next_minute({YMD,{H,I,S}}) ->
    {YMD, {H,I+1,S}}.

%% @doc Return the date one second later.
next_second({_,{_,_,59}} = Date) ->
    {YMD,{H,I,_}} = next_minute(Date),
    {YMD,{H,I,0}};
next_second({YMD,{H,I,S}}) ->
    {YMD, {H,I,S+1}}.

%% ====================================================================

validate_time({HH,MM,SS}) ->
    validate_time(HH,MM,SS).
validate_time(HH,MM,SS) ->
    HH>=0 andalso HH=<23 andalso
        MM>=0 andalso MM=<60 andalso
        SS>=0 andalso SS=<60.

%% ====================================================================

get_datetime_year({{Value,_,_},_}) ->
    Value.
get_datetime_month({{_,Value,_},_}) ->
    Value.
get_datetime_day({{_,_,Value},_}) ->
    Value.
get_datetime_week({Date,_}) ->
    calendar:day_of_the_week(Date).
get_datetime_hour({_,{Value,_,_}}) ->
    Value.
get_datetime_minute({_,{_,Value,_}}) ->
    Value.
get_datetime_second({_,{_,_,Value}}) ->
    Value.

%% ====================================================================
get_datetime_by_type(year,{{Value,_,_},_}) ->
    Value;
get_datetime_by_type(month,{{_,Value,_},_}) ->
    Value;
get_datetime_by_type(day,{{_,_,Value},_}) ->
    Value;
get_datetime_by_type(week,{Date,_}) ->
    calendar:day_of_the_week(Date);
get_datetime_by_type(hour,{_,{Value,_,_}}) ->
    Value;
get_datetime_by_type(minute,{_,{_,Value,_}}) ->
    Value;
get_datetime_by_type(second,{_,{_,_,Value}}) ->
    Value.
