ERL_SOURCES = erl_parse.erl erlson_parse_transform.erl erlson.erl erlson_tests.erl
TEST = erlson_tests

ERL = erl
ERLC = erlc
EBIN_DIR = $(PWD)
ERLC_FLAGS = -pa $(EBIN_DIR)

ERL_OBJECTS = $(ERL_SOURCES:%.erl=$(EBIN_DIR)/%.beam)

.PHONY: all test


all: $(ERL_SOURCES) $(ERL_OBJECTS) expand test 


erlson_tests.erl: erlson_parse_transform.erl erlson.erl
	#cp ../test/$@ $@
	touch -r $< $@


$(ERL_OBJECTS): %.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<


%.erl: %.yrl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<


test: $(ERL_OBJECTS)
	$(ERL) -pa . -I . -noshell -s eunit test $(TEST) -s erlang halt #init stop


expand: $(ERL_OBJECTS) erlson_tests.P

%.P: %.erl
	$(ERL) -pa . -I . -noshell -eval "compile:file($<,'P')" -s erlang halt #init stop


clean:
	rm -f $(ERL_OBJECTS) erl_crash.dump erl_parse.erl erlson_tests.P # erlson_tests.erl 

