-module(ecrontab_clock_tick).
-behaviour(gen_server).
-include("ecrontab.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
    start_link/0
]).

-record(state, {seconds, tref}).
-define(START_TASK_TIMER, timer:send_interval(1000, self(), tick)).

%% ====================================================================
%% API
%% ====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ====================================================================
%% callback API
%% ====================================================================

init(_Args) ->
    process_flag(trap_exit, true),
    Seconds = calendar:datetime_to_gregorian_seconds(erlang:localtime()),
    {ok ,TRef} = ?START_TASK_TIMER,
    {ok, #state{seconds = Seconds, tref = TRef}}.

handle_call(_Msg, _From, State) ->
    {reply, noknow, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick, State) ->
    Seconds = State#state.seconds+1,
%%    NowSeconds = calendar:datetime_to_gregorian_seconds(erlang:localtime()),
%%    io:format("diff Seconds:~p~n",[NowSeconds-Seconds]),
    ecrontab_task_manager:tick(Seconds),
    PidList = pg2:get_members(?GROUP_NAME),
    loop_send(PidList, Seconds),
    {noreply, #state{seconds = Seconds}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    timer:cancel(State#state.tref),
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

loop_send([Pid|PidList], Seconds) ->
    Pid ! {ecrontab_tick, Seconds},
    loop_send(PidList, Seconds);
loop_send([], _) ->
    ok.