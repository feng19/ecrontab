
%% spec_field_type
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
    type :: ecrontab:spec_field_type(),
    value :: ecrontab:spec_field_value()
}).

-record(spec, {
    type :: ecrontab:spec_type(),
    value :: any(),
    year :: ecrontab:spec_field(),
    month :: ecrontab:spec_field(),
    day :: ecrontab:spec_field(),
    week :: ecrontab:spec_field(),
    hour :: ecrontab:spec_field(),
    minute :: ecrontab:spec_field(),
    second :: ecrontab:spec_field()
}).

%% spec_type
-define(SPEC_TYPE_NORMAL,           normal).
-define(SPEC_TYPE_TIMESTAMP,        timestamp).
-define(SPEC_TYPE_EVERY_SECOND,     every_second).
-define(SPEC_TYPE_INTERVAL_YEAR,    interval_year).
-define(SPEC_TYPE_ONLY_ONE,         only_one).