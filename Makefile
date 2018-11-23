STYLE=--name-mixed-case --based-grouping=3 -D$(PWD)/dict
GFLAGS=-O3 -gnaty -gnatwa

all:: check getr

check:: env_check
	./env_check
	gnatpp $(STYLE) --output=getr.pp getr.adb
	diff -q getr.pp getr.adb

clean::
	gnat clean getr.adb
	rm -fv env_check

getr: getr.adb
	gnat make $(GFLAGS) $<

env_check: env_check.c
	gcc -O3 -Wall -o $@ $<
