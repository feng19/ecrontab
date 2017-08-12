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
    {ok, TRef} = ?START_TASK_TIMER,
    {ok, #state{seconds = ?TIMESTAMP, tref = TRef}}.

handle_call(_Msg, _From, State) ->
    {reply, noknow, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick, State) ->
    Seconds = State#state.seconds + 1,
    gproc:send(?GROUP_NAME, {ecrontab_tick, Seconds}),
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