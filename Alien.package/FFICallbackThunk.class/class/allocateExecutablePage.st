executable page management
allocateExecutablePage "^<Alien>"
	"Answer an Alien around a piece of (probably malloced) memory, of some multiple
	 of the pagesize, that has execute permissions set.  This memory can be parcelled
	 out to individual FFICallbackThunks to hold their thunk/trampoline machine code."
	<primitive: 'primAllocateExecutablePage' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed