-ifndef(ECRONTAB_H).
-define(ECRONTAB_H, true).

-define(GROUP_NAME, {p, l, ecrontab_group}).

-define(ONE_PROCESS_MAX_TASKS_COUNT, 1000).

-define(ETS_WORKER_NAME_INDEX, ets_worker_name_index). %% {name, worker_pid}

-record(task, {
    name :: ecrontab:task_name(),
    spec :: ecrontab:spec(),
    mfa :: ecrontab:ecrontab_mfa(),
    add_time :: calendar:datetime(),
    options
}).

-define(TIMESTAMP, erlang:system_time(seconds)).
-define(MILLI_TIMESTAMP, erlang:system_time(millisecond)).
-define(MICRO_TIMESTAMP, erlang:system_time(microsecond)).

-define(DATETIME_TO_TIMESTAMP(Datetime), ecrontab_time_util:datetime_to_timestamp(Datetime)).
-define(TIMESTAMP_TO_DATETIME(Timestamp), ecrontab_time_util:timestamp_to_datetime(Timestamp)).

-endif.