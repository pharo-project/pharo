accessing
statusIfStopped
	"
	/* If WIFSTOPPED(STATUS), the signal that stopped the child.  */
	#define __WSTOPSIG(status)      __WEXITSTATUS(status)
	"
	^self statusIfExited