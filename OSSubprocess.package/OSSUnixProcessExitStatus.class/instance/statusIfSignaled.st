accessing
statusIfSignaled
	"
	/* If WIFSIGNALED(STATUS), the terminating signal.  */
	#define __WTERMSIG(status)      ((status) & 0x7f)
	"
	^intValue bitAnd: 16r7F