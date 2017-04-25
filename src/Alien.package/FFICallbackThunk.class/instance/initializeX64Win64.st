instance initalization
initializeX64Win64
	"Initialize the receiver with a __ccall thunk."
	"thunk+0x0:	pushq  %rsp							54
	 thunk+0x1:	pushq  %rsp							54
	 thunk+0x4:	movabsq $thunk, %rax					48 b8 b0..b7 eight bytes of thunk address a.k.a. addressField
	 thunk+0xc:	pushq  %rax							50
	 thunk+0xd:	subq   $0x20, %rsp						48 83 c4 e0 (this is addq -20 since the immediate is signed extended)
	 thunk+0x11:	movabsq $thunkEntry, %rax			48 b8 b0..b7 eight bytes of the thunkEntry address
	 thunk+0x1b:	callq  *%rax							ff d0
	 thunk+0x1d:	addq   $0x38, %rsp					48 83 c4 38
	 thunk+0x21:	retq									c3
	 thunk+0x22:	nop										90
	 thunk+0x23:	nop										90"
	self
		unsignedLongAt:		  1 put: 16rb8485454;
		unsignedLongLongAt:	  5 put: self addressField;
		unsignedLongAt:		13 put: 16rc4834850; 
		unsignedLongAt:		17 put: 16r00b848e0; "00 is the first byte of the 64-bit constant the movabsq/0x48 opcode moves"
		unsignedLongLongAt:	20 put: self primThunkEntryAddress;
		unsignedByteAt:		28 put: 16rff;
		unsignedLongAt:		29 put: 16rc48348d0;
		unsignedLongAt:		33 put: 16r9090c338.
	self primThunkEntryAddress

	"self newCCall"