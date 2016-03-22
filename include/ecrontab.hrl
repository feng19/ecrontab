
-define(GROUP_NAME, ecrontab_group).

-define(ONE_PROCESS_MAX_TASKS_COUNT, 50000).

-define(ETS_NAME_TASK_INDEX, ets_task_index).

-record(task_index, {name, tid}).
-record(task, {name, spec, mfa, add_time, options}).
