.PHONY: compile test test-latest clean

compile:
	@ rebar compile

test: compile
	@ ./run_test 0.9.0 && \
	  ./run_test 0.8.6 && \
	  ./run_test 0.8.5

test-latest: compile
	@ ./run_test 0.9.0

clean:
	@ rebar clean
	@ rm -rf deps/cowboy
	@ rm -rf deps/cowlib
	@ rm -rf deps/ranch
