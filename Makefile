STYLE=--name-mixed-case --based-grouping=3 -D$(PWD)/dict

all:: check getr

check:: env_check
	./env_check
	gnatpp $(STYLE) --output=getr.pp getr.adb
	diff -q getr.pp getr.adb

clean::
	gnat clean getr.adb
	rm -fv env_check

getr: getr.adb
	gnat make -gnaty $<

env_check: env_check.c
	gcc -O3 -Wall -o $@ $<
