testing
isStopped
	"
	/* Nonzero if STATUS indicates the child is stopped.  */
	#define __WIFSTOPPED(status)    (((status) & 0xff) == 0x7f)
	"
	^(intValue bitAnd: 16rFF) = 16r7F

