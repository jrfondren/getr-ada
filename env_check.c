#include <sys/resource.h>
#include <sys/time.h>
#include <stdio.h>
#include <assert.h>

int main (void) {
	/* is this hardcoded constant wrong? */
	assert(RUSAGE_CHILDREN == -1);

	/* is this not a 64-bit system? */
	assert(sizeof(long) == 8);

	/* is timeval not two 64-bit numbers? */
	assert(sizeof(struct timeval) == 16);

	/* does rusage have 'reserved' padding or new fields? */
	assert(sizeof(struct rusage) == (sizeof(long) * (2 + 2 + 14)));
	return 0;
}
