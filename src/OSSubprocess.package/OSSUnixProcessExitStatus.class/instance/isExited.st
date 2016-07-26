testing
isExited
	"
	/* Nonzero if STATUS indicates normal termination.  */
	#define __WIFEXITED(status)     (__WTERMSIG(status) == 0)
	"
	^ self statusIfSignaled = 0

