instance initalization
initializeX64
	"Initialize the receiver with a __ccall thunk."
	"thunk+0x0:	pushq  %rsp							54
	 thunk+0x1:	pushq  %rsp							54
	 thunk+0x4:	movabsq $thunk, %rax					48 b8 b0..b7 eight bytes of thunk address a.k.a. addressField
	 thunk+0xc:	pushq  %rax							50
	 thunk+0xd:	movabsq $thunkEntry, %rax			48 b8 b0..b7 eight bytes of the thunkEntry address
	 thunk+0x17:	callq  *%rax							ff d0
	 thunk+0x19:	addq   $0x18, %rsp					48 83 c4 18
	 thunk+0x1d:	retq									c3
	 thunk+0x1e:	nop										90
	 thunk+0x1f:	nop										90"
	self
		unsignedLongAt:		  1 put: 16rb8485454;
		unsignedLongLongAt:	  5 put: self addressField;
		unsignedLongAt:		13 put: 16r00b84850; "00 is the first byte of the 64-bit constant the movabsq/0x48 opcode moves"
		unsignedLongLongAt:	16 put: self primThunkEntryAddress;
		unsignedByteAt:		24 put: 16rff;
		unsignedLongAt:		25 put: 16rc48348d0;
		unsignedLongAt:		29 put: 16r9090c318.
	self primThunkEntryAddress

	"self newCCall"