compile:
	rebar compile

build-plt:
	dialyzer --build_plt \
	-pa deps/proper/ebin \
	--output_plt compression_dialyzer.plt \
	--apps kernel crypto stdlib sasl

dialyze: compile
	dialyzer --plt compression_dialyzer.plt \
	-c ebin
	-pa deps/proper/ebin
