-module(ecrontab).
-export([
    add/3, add/4,
    remove/1, remove/2,
    parse_spec/1
]).

%% ====================================================================

add(Name, Spec, MFA) ->
    add(Name, Spec, MFA, []).
add(Name, Spec, MFA, Options) ->
    ecrontab_server:add(Name, Spec, MFA, Options).

remove(Name) ->
    remove(Name, []).
remove(Name, Options) ->
    ecrontab_server:remove(Name, Options).

parse_spec(Spec) ->
    ecrontab_parse:parse_spec(Spec).

%% ====================================================================
