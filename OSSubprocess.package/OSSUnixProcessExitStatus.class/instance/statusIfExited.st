accessing
statusIfExited
	"
	/* If WIFEXITED(STATUS), the low-order 8 bits of the status.  */
	#define __WEXITSTATUS(status)   (((status) & 0xff00) >> 8)
	"
	^(intValue bitAnd: 16rFF00) >> 8