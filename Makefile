REBAR ?= ./rebar


.PHONY: all compile test clean


all: compile


compile:
	$(REBAR) compile


test: compile
	$(REBAR) eunit


clean:
	$(REBAR) clean -r

