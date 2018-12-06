STYLE=--name-mixed-case --based-grouping=3 --par-threshold=3 --call-threshold=3 -D$(PWD)/dict
GFLAGS=-O3 -gnaty -gnatwa

all:: config check getr

config::
	[[ "$$(uname)" == "Darwin" ]] && perl -i -pe 's/False/True/ if /\b macOS \b/x' build_options.ads

check:: env_check
	./env_check
	gnatpp $(STYLE) --output-force=getr.pp getr.adb
	diff -q getr.pp getr.adb
	rm -fv getr.pp

clean::
	gnat clean getr.adb
	rm -fv env_check

getr: getr.adb
	gnat make $(GFLAGS) $<

env_check: env_check.c
	gcc -O3 -Wall -o $@ $<
