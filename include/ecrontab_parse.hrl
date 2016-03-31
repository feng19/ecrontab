
-define(SPEC_FIELD_TYPE_ANY,        any).
-define(SPEC_FIELD_TYPE_NUM,        num).
-define(SPEC_FIELD_TYPE_LIST,       list).
-define(SPEC_FIELD_TYPE_INTERVAL,   interval).

-define(SPEC_FIELD_ANY,   any).

-define(POS_YEAR,   #spec.year).
-define(POS_MONTH,  #spec.month).
-define(POS_DAY,    #spec.day).
-define(POS_WEEK,   #spec.week).
-define(POS_HOUR,   #spec.hour).
-define(POS_MINUTE, #spec.minute).
-define(POS_SECOND, #spec.second).

%% ====================================================================

-record(spec_field, {
    type :: spec_field_type(),
    value :: spec_field_value()
}).

-record(spec, {
    type :: spec_type(),
    value :: any(),
    year :: spec_field(),
    month :: spec_field(),
    day :: spec_field(),
    week :: spec_field(),
    hour :: spec_field(),
    minute :: spec_field(),
    second :: spec_field()
}).

-define(SPEC_TYPE_NORMAL,           normal).
-define(SPEC_TYPE_SECONDS,          seconds).
-define(SPEC_TYPE_EVERY_SECOND,     every_second).
-define(SPEC_TYPE_INTERVAL_YEAR,    interval_year).
-define(SPEC_TYPE_ONLY_ONE,         only_one).

%% ====================================================================

-type spec_type() :: term().
          
-type spec_field_type() :: 
          ?SPEC_FIELD_TYPE_ANY | 
          ?SPEC_FIELD_TYPE_NUM | 
          ?SPEC_FIELD_TYPE_LIST | 
          ?SPEC_FIELD_TYPE_INTERVAL.

-type spec_field_any() :: ?SPEC_FIELD_ANY.

-type spec_field_num() :: non_neg_integer().

-type spec_field_list() :: [spec_field_num()].

-type spec_field_interval() :: non_neg_integer().

-type spec_field_value() :: 
          spec_field_any() | 
          spec_field_num() | 
          spec_field_list() | 
          spec_field_interval().

-type spec_field() :: #spec_field{}.

-type spec() :: #spec{}.