
-define(GROUP_NAME, ecrontab_group).

-define(ONE_PROCESS_MAX_TASKS_COUNT, 1000).

-define(ETS_WORKER_NAME_INDEX, ets_worker_name_index). %% {name, worker_pid}

-record(task, {spec, mfa, add_time, options}).
