REBAR = $(shell command -v rebar || echo ./rebar)
DEPS_PLT=./.deps_plt
DEPS=erts kernel stdlib inets crypto mnesia public_key ssl
DIALYZER = dialyzer

DIALYZER_WARNINGS = -Wunmatched_returns -Werror_handling \
                    -Wrace_conditions -Wunderspecs

.PHONY: all compile test qc clean get-deps build-plt dialyze

all: compile

deps:
	@$(REBAR) get-deps
	@$(REBAR) compile

compile:
	@$(REBAR) compile

test: compile
	@$(REBAR) eunit skip_deps=true

clean:
	@$(REBAR) clean

get-deps:
	@$(REBAR) get-deps

$(DEPS_PLT):
	@echo Building $(DEPS_PLT)
	@$(DIALYZER) --build_plt \
		-r deps \
		--output_plt $(DEPS_PLT) \
		--apps $(DEPS)

dialyze: $(DEPS_PLT) compile
	@$(DIALYZER) --fullpath \
		-Wunmatched_returns \
		-Werror_handling \
		-Wrace_conditions \
		-Wunderspecs \
		--plt $(DEPS_PLT) \
		ebin | grep -vf .dialyzerignore

xref:
	@$(REBAR) xref
