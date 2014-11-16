REBAR ?= rebar


.PHONY: all compile test clean


all: compile


compile:
	rebar compile


test: compile
	rebar eunit


clean:
	rebar clean -r

