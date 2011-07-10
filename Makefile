.PHONY: all test

all:
	./rebar compile

test:
	./rebar eunit
