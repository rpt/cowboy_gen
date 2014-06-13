.PHONY: compile test test-latest clean

compile:
	@ rebar compile

test: compile
	@ ./run_test 0.10.0 && \
	  ./run_test 0.9.0 && \
	  ./run_test 0.8.6 && \
	  ./run_test 0.8.5 && \
	  ./run_test 0.8.4 && \
	  ./run_test 0.8.3 && \
	  ./run_test 0.8.2 && \
	  ./run_test 0.8.1 && \
	  ./run_test 0.8.0

test-latest: compile
	@ ./run_test 0.10.0

clean:
	@ rebar clean
	@ rm -rf deps/cowboy
	@ rm -rf deps/cowlib
	@ rm -rf deps/ranch
	@ rm -f rebar.tmp.config
