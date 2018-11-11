all:: check getr

check:: env_check
	./env_check

clean::
	gnat clean getr.adb
	rm -fv env_check

getr: getr.adb
	gnat make $<

env_check: env_check.c
	gcc -O3 -Wall -o $@ $<
