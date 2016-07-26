testing
isSignaled
	"
	/* Nonzero if STATUS indicates termination by a signal.  */
	#define __WIFSIGNALED(status) \
	  (((signed char) (((status) & 0x7f) + 1) >> 1) > 0)
	"
	^(((intValue bitAnd: 16r7F) + 1) >> 1) > 0

