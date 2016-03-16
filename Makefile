
PROJECT = ecrontab
#DEPS = lager 
#dep_lager = git https://github.com/basho/lager master

#ERLC_OPTS += -Ddebug
#ERLC_OPTS += +debug_info
ERLC_OPTS += +no_debug_info
#ERLC_OPTS += +'{parse_transform, lager_transform}'

COVER = true

include erlang.mk

