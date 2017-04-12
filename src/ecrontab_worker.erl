-module(ecrontab_worker).
-behaviour(gen_server).
-include("ecrontab.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
    start_link/1,
    add/2
]).

-record(state, {name, task_count = 0, max_task_count = 0, now_seconds}).
-record(server_task, {spec, mfa, options}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

add(Pid, Task) ->
    try
        gen_server:call(Pid, {add, Task})
    catch
        {'EXIT', Reason} ->
            {error, Reason}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([WorkerName]) ->
    init([WorkerName, ?ONE_PROCESS_MAX_TASKS_COUNT]);
init([WorkerName, MaxTaskCount]) ->
    case ets:insert_new(?ETS_WORKER_NAME_INDEX, {WorkerName, self()}) of
        true ->
            process_flag(trap_exit, true),
            pg2:join(?GROUP_NAME, self()),
            {ok, #state{name = WorkerName, now_seconds = ?TIMESTAMP, max_task_count = MaxTaskCount}};
        false ->
            {stop, duplicate_name}
    end.

handle_call({add, _Task}, _From, State) when State#state.task_count == State#state.max_task_count ->
    {reply, {error, worker_over}, State};
handle_call({add, Task}, _From, State) ->
    {Reply, NewState} = do_add(State, Task),
    {reply, Reply, NewState};
handle_call(_Msg, _From, State) ->
    {reply, noknow, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({ecrontab_tick, Seconds}, State) ->
    NewState = do_tick(State#state{now_seconds = Seconds}),
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ets:delete(?ETS_WORKER_NAME_INDEX, State#state.name),
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.

%% ====================================================================
%% internal API
%% ====================================================================

do_tick(State) ->
    case erlang:erase(State#state.now_seconds) of
        undefined ->
            State;
        [] ->
            State;
        STasks ->
            NowDatetime = ?TIMESTAMP_TO_DATETIME(State#state.now_seconds),
            loop_tasks(NowDatetime, STasks, State)
    end.

loop_tasks(_, [], State) ->
    State;
loop_tasks(NowDatetime, [STask | STasks], State) ->
    do_spawn_task(STask#server_task.mfa),
    case ecrontab_next_time:next_seconds(STask#server_task.spec, NowDatetime) of
        {ok, NextSeconds} ->
            put_in_list(NextSeconds, STask),
            loop_tasks(NowDatetime, STasks, State);
        _ ->
            loop_tasks(NowDatetime, STasks, State#state{task_count = State#state.task_count - 1})
    end.

do_spawn_task({M, F, A}) ->
    erlang:spawn(M, F, A);
do_spawn_task({Node, M, F, A}) ->
    rpc:cast(Node, M, F, A);
do_spawn_task(Fun) ->
    erlang:spawn(Fun).

do_add(State, Task) ->
    NowDatetime = ?TIMESTAMP_TO_DATETIME(State#state.now_seconds),
    case ecrontab_next_time:next_seconds(Task#task.spec, NowDatetime) of
        {ok, NextSeconds} ->
            STask = task_to_server_task(Task),
            put_in_list(NextSeconds, STask),
            {ok, State#state{task_count = State#state.task_count + 1}};
        Err ->
            {Err, State}
    end.

task_to_server_task(Task) ->
    #server_task{spec = Task#task.spec, mfa = Task#task.mfa, options = Task#task.options}.

put_in_list(Seconds, STask) ->
    List =
        case get(Seconds) of
            undefined ->
                [STask];
            List0 ->
                [STask | List0]
        end,
    put(Seconds, List).