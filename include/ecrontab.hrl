
-define(GROUP_NAME, ecrontab_group).

-define(ONE_PROCESS_MAX_TASKS, 10000).

-define(ETS_NAME_TASKS, ets_tasks).

-record(task, {name, spec, mfa, options}).
-record(next_time, {seconds, tasks}).