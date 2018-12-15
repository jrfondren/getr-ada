# getrusage() wrapper
- known to work on Linux
- created as my simple "time for x in {1..100}; ..." benchmarks were a lot less pleasant on OpenBSD.

## Ada notes
- this is an Ada translation of the C version at https://github.com/jrfondren/getr
- I'm *not* well versed in Ada
- glibc's sys/resource.h is such that gcc's own translator created nearly useless Ada code from it: https://gist.github.com/jrfondren/1d98e9dcc056184f1317a1ac47e59673
- the 'florist' package of POSIX extensions to Ada would've been useful, but that package doesn't seem to be available on OpenBSD
- `GNAT.OS_Lib.Spawn` and friends are incapable of running a command with no arguments (although the OS always expects the command itself as its first argument, libGNAT already supplies that first argument for you)

## build
```
make
```

## RTFM
```
man ./getr.mdoc
```

## usage and examples
```
$ getr 1000 ./fizzbuzz >/dev/null
User time      : 0 s, 307494 us
System time    : 0 s, 160173 us
Time           : 467 ms (0.467 ms/per)
Max RSS        : 1684 kB
Page reclaims  : 65338
Page faults    : 0
Block inputs   : 0
Block outputs  : 0
vol ctx switches   : 1004
invol ctx switches : 29

$ getr 100 $(which python3) -c ''
User time      : 1 s, 451823 us
System time    : 0 s, 274621 us
Time           : 1726 ms (17.260 ms/per)
Max RSS        : 8704 kB
Page reclaims  : 98324
Page faults    : 0
Block inputs   : 0
Block outputs  : 0
vol ctx switches   : 102
invol ctx switches : 12

$ getr 100 $(which perl) -le ''
User time      : 0 s, 76980 us
System time    : 0 s, 69690 us
Time           : 146 ms (1.460 ms/per)
Max RSS        : 5072 kB
Page reclaims  : 22209
Page faults    : 0
Block inputs   : 0
Block outputs  : 0
vol ctx switches   : 102
invol ctx switches : 10
```

## defects and room for improvement
- no $PATH resolution occurs
- output is in an ad-hoc text format that machine consumers would need to parse manually
- only posix_spawn is used, but fork&exec might be preferred for timings more like a fork&exec-using application
- 'getr' is probably a poor name
- kB and ms are always used even when number ranges might be easier to understand in MB or s, or GB or min:s
