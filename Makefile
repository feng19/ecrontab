all: compile test

.PHONY: co compile test

## 编译
co:compile
compile:
	rebar3 compile

test:
	rebar3 do eunit, ct, cover