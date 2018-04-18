all: compile test

.PHONY: co compile test elvis dialyzer xref

## 编译
co:compile
compile:
	rebar3 compile

test:
	rebar3 do eunit, ct, cover -v

eunit:
	rebar3 do eunit -v, cover -v

ct:
	rebar3 do ct -v, cover -v

elvis:
	elvis rock

dialyzer:
	rebar3 dialyzer

xref:
	rebar3 xref