primitives-memory management
primFree
	"Free the memory referenced by the receiver, or fail if
	 the receiver is direct or the indirect pointer is invalid.
	 Note that if successful the address and size fields are
	 zero'ed, effectively preventing double-freeing hazards."
	<primitive: 'primBoxedFree' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed