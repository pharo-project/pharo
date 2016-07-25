private-primitives
primThunkEntryAddress "^<Integer>"
	"Answer the address of the entry-point for thunk callbacks:
		x86: long thunkEntry(void *thunkp, long *stackp);
		x64: long thunkEntry(long a, long b, long c, long d, long, e, long f,
								double d0, double d1, double d2, double d3,
								double d4, double d5, double d6, double d7,
								void *thunkp, long *stackp);
		ARM: long thunkEntry(long a, long b, long c, long d,
								double d0, double d1, double d2, double d3,
								double d4, double d5, double d6, double d7,
								void *thunkp, long *stackp);
	 etc.
	 This is the function a callback thunk/trampoline should call to initiate a callback."
	<primitive: 'primThunkEntryAddress' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed