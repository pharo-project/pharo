"
An instance of FFICallbackThunk is a reference to a machine-code thunk/trampoline that calls-back into the VM.  The reference can be passed to C code which can use it as a function pointer through which to call-back into Smalltalk.  The machine-code thunk/trampoline is different for each instance, hence its address is a unique key that can be used to assocuate the Smalltalk side of the call-back (e.g. a block) with the thunk.  Since thunks must be executable and some OSs may not provide default execute permission on memory returned by malloc we may not be able to use malloc directly.  Instead we rely on a primitive to provide memory that is guaranteed to be executable.  The FFICallbackThunk class>>allocateExectablePage primitive answers an Alien that references an executable piece of memory that is some (possiby unitary) multiple of the pagesize.  Class-side code then parcels out pieces of a page to individual thunks.  These pieces are recycled when thunks are reclaimed.  Since the first byte of a thunk is non-zero we can use it as a flag indicating if the piece is in use or not.

See Callback for the higher-level construct that represents a Smalltalk block to be run in response to a callback.  Callbacks wrap instances of FFICallbackThunk and VMCallbackContext[32|64] instances that describe the stack layout and register contents for receiving callback arguments.

Class Variables
AccessProtect <Semaphore> critical section for ExecutablePages (de)allocation
AllocatedThunks <AlienWeakTable of <FFICallbackThunk -> Integer>> - weak collection of thunks, used to return thunk storage to the executable page pool.
ExecutablePages <Set of: Alien ""executable page""> - collection of pages with execute permissions used to provide executable thunks
"
Class {
	#name : #FFICallbackThunk,
	#superclass : #Alien,
	#type : #bytes,
	#classVars : [
		'AccessProtect',
		'AllocatedThunks',
		'ExecutablePages'
	],
	#category : #'Alien-Core'
}

{ #category : #'executable page management' }
FFICallbackThunk class >> allocateExecutablePage [ "^<Alien>"
	"Answer an Alien around a piece of (probably malloced) memory, of some multiple
	 of the pagesize, that has execute permissions set.  This memory can be parcelled
	 out to individual FFICallbackThunks to hold their thunk/trampoline machine code."
	<primitive: 'primAllocateExecutablePage' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed
]

{ #category : #'instance creation' }
FFICallbackThunk class >> dataSize [
	^40 "see FFICallbackThunk initialize & initializeStdcall:; must be big enough for the largest thunk created."
]

{ #category : #'class initialization' }
FFICallbackThunk class >> ensureNotifiedAtStartup [
	"FFICallbackThunk needs to be notified at system startup to clear the ExecutablePages table."
	self ~~ FFICallbackThunk ifTrue: [^self].
	(Smalltalk classNamed: #SessionManager)
		ifNotNil: [:sessionManagerClass|
			sessionManagerClass default
				registerSystemClassNamed: self name
				atPriority: 60]
		ifNil: [Smalltalk addToStartUpList: self after: Alien]
]

{ #category : #finalization }
FFICallbackThunk class >> finalize: address [
	self freeBlockInExecutablePage: address
]

{ #category : #'executable page management' }
FFICallbackThunk class >> freeBlockInExecutablePage: address [
	ExecutablePages do:
		[:alienPage| | alienAddress |
		alienAddress := alienPage address.
		(address >= alienAddress
		 and: [alienAddress + alienPage dataSize >= address]) ifTrue:
			[alienPage unsignedByteAt: address - alienAddress + 1 put: 0.
			 ^self]]
			
]

{ #category : #'class initialization' }
FFICallbackThunk class >> initialize [
	"FFICallbackThunk initialize"
	AccessProtect := Semaphore forMutualExclusion.
	AllocatedThunks := AlienWeakTable newForOwner: self.
	ExecutablePages := Set new.
	self ensureNotifiedAtStartup
]

{ #category : #'instance creation' }
FFICallbackThunk class >> newCCall [
	"Answer a new instance for the C calling-convention."
	^self privateFinalizingNew initialize
]

{ #category : #'instance creation' }
FFICallbackThunk class >> newStdCall: argBytes [
	"Answer a new instance for Windows' StdCall (Pascal) convention, given the
	 size of the arguments in bytes (bytes to cut-back from the stack on return)."
	^self privateFinalizingNew initializeStdcall: argBytes
]

{ #category : #'instance creation' }
FFICallbackThunk class >> privateFinalizingNew [
	"Answer a new instance that will free its page when garbage collected."
	| address |
	address := self unusedBlockInExecutablePage.
	^AllocatedThunks
		add: (self forPointer: address)
		finalizing: address 
]

{ #category : #'class initialization' }
FFICallbackThunk class >> startUp: inANewImageSession [
	"Any thunks in the finalization registry at the time the image comes up in a new session MUST NOT be finalized and should  immediately be discarded. Their thunk pointers are no longer valid."
	inANewImageSession ifTrue:
		[AllocatedThunks removeAll.
		ExecutablePages := Set new]
]

{ #category : #'executable page management' }
FFICallbackThunk class >> unusedBlockInExecutablePage [
	AccessProtect critical:
		[ExecutablePages do:
			[:alien|
			1 to: alien dataSize - self dataSize by: self dataSize do:
				[:i|
				(alien unsignedByteAt: i) = 0 ifTrue:
					[alien unsignedByteAt: i put: 1.
					 ^alien address + i - 1]]]].
	^(AccessProtect critical: [ExecutablePages add: self allocateExecutablePage])
		unsignedByteAt: 1 put: 1;
		address
]

{ #category : #accessing }
FFICallbackThunk >> address [
	"Thunks are hacked - again they need to be indirect objects with a 0 size field."
	^self addressField
]

{ #category : #'memory management' }
FFICallbackThunk >> free [
	"Recycle the memory in the executable page containing the receiver's thunk.
	 Since the receiver is a derived pointer to a block in an executable page this
	 requires returning the block to the free pool."
	AllocatedThunks remove: self.
	AccessProtect critical:
		[| address |
		address := self addressField.
		address ~= 0 ifTrue:
			[self addressFieldPut: 0.
			 self class freeBlockInExecutablePage: address]]
]

{ #category : #initialization }
FFICallbackThunk >> initialize [
	"Initialize the receiver with a __ccall thunk."
	Callback abi caseOf: {
	['IA32']		->	[self initializeX86].
	['ARM32']	->	[self initializeARM32].
	['X64']		->	[self initializeX64].
	['X64Win64']	->	[self initializeX64Win64] }
]

{ #category : #'instance initialization' }
FFICallbackThunk >> initializeARM32 [
	"Initialize the receiver with a __ccall thunk.  The thunk calls thunkEntry in the Alien/IA32ABI plugin,
	 whose source is in platforms/Cross/plugins/IA32ABI/arm32abicc.c.  thunkEntry is the entry point
	 for Callbacks. The objective of the thunk is to call thunkEntry with all arguments to the call of the
	 thunk (registers and stack) as well as a pointer to the thunk itself.  thunkEntry is as follows:
	
		long long
		thunkEntry(long r0, long r1, long r2, long r3,
		            double d0, double d1, double d2, double d3,
		            double d4, double d5, double d6, double d7,
		            void *thunkpPlus16, sqIntptr_t *stackp)

	thunkEntry then collects the various arguments (thunk, integer register arguments, floating-point
	register arguments, stack pointer) in a VMCallbackContext and invokes the callback via invokeCallbackContext:."

	"0x0 <thunk+0>:		mov     r12, sp			; 0xe1a0c00d
 	 0x4 <thunk+4>:		sub     sp, sp, #16		; 0xe24dd010
 	 0x8 <thunk+8>:		str     pc, [sp, #0]		; 0xe58df000 N.B. passes thunk+16; thunkEntry compensates
 	 0xc <thunk+12>:		str     r12, [sp,#4]		; 0xe58dc004
 	 0x10 <thunk+16>:	str     lr, [sp, #12]		; 0xe58de00c
 	 0x14 <thunk+20>:	ldr     r12, [pc, #8]		; 0xe59fc008
 	 0x18 <thunk+24>:	blx     r12				; 0xe12fff3c
 	 0x1c <thunk+28>:	add     sp, sp, #12		; 0xe28dd00c
 	 0x20 <thunk+32>:	ldr     pc, [sp], #4!		; 0xe49df004 ; pop     {pc}
 	 0x24 <thunk+36>:     .word thunkEntry"
	self
		unsignedLongAt:   1 put: 16re1a0c00d;
		unsignedLongAt:   5 put: 16re24dd010;
		unsignedLongAt:   9 put: 16re58df000;
		unsignedLongAt: 13 put: 16re58dc004;
		unsignedLongAt: 17 put: 16re58de00c;
		unsignedLongAt: 21 put: 16re59fc008;
		unsignedLongAt: 25 put: 16re12fff3c;
		unsignedLongAt: 29 put: 16re28dd00c;
		unsignedLongAt: 33 put: 16re49df004;
		unsignedLongAt: 37 put: self primThunkEntryAddress

	"self newCCall"
]

{ #category : #'instance initialization' }
FFICallbackThunk >> initializeStdcall: bytes [
	"Initialize the receiver with a __stdcall thunk with bytes argument bytes."
	Callback abi caseOf: {
	['IA32']		->	[self initializeX86Stdcall: bytes] }
]

{ #category : #'instance initialization' }
FFICallbackThunk >> initializeX64 [
	"Initialize the receiver with a __ccall thunk.  The thunk calls thunkEntry in the Alien/IA32ABI plugin,
	 whose source is in platforms/Cross/plugins/IA32ABI/x64sysvabicc.c.  thunkEntry is the entry point
	 for Callbacks. The objective of the thunk is to call thunkEntry with all arguments to the call of the
	 thunk (registers and stack) as well as a pointer to the thunk itself.  thunkEntry is as follows:

		long
		thunkEntry(long a0, long a1, long a2, long a3, long a4, long a5,
		            double d0, double d1, double d2, double d3,
		            double d4, double d5, double d6, double d7,
		            void *thunkp, sqIntptr_t *stackp)

	thunkEntry then collects the various arguments (thunk, integer register arguments, floating-point
	register arguments, stack pointer) in a VMCallbackContext and invokes the callback via invokeCallbackContext:."

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
]

{ #category : #'instance initialization' }
FFICallbackThunk >> initializeX64Win64 [
	"Initialize the receiver with a __ccall thunk.  The thunk calls thunkEntry in the Alien/IA32ABI plugin,
	 whose source is in platforms/Cross/plugins/IA32ABI/x64win64abicc.c.  thunkEntry is the entry point
	 for Callbacks. The objective of the thunk is to call thunkEntry with all arguments to the call of the
	 thunk (registers and stack) as well as a pointer to the thunk itself.  thunkEntry is as follows:

		long long
		thunkEntry(long long rcx, long long rdx,
		      		long long r8, long long r9,
					void *thunkp, sqIntptr_t *stackp)

	thunkEntry then collects the various arguments (thunk, integer register arguments, stack pointer)
	in a VMCallbackContext and invokes the callback via invokeCallbackContext:."

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
]

{ #category : #'instance initialization' }
FFICallbackThunk >> initializeX86 [
	"Initialize the receiver with a __ccall thunk.  The thunk calls thunkEntry in the Alien/IA32ABI plugin,
	 whose source is in platforms/Cross/plugins/IA32ABI/x64win64abicc.c.  thunkEntry is the entry point
	 for Callbacks. The objective of the thunk is to call thunkEntry with all arguments to the call of the
	 thunk (registers and stack) as well as a pointer to the thunk itself.  thunkEntry is as follows:

		long
		thunkEntry(void *thunkp, sqIntptr_t *stackp)

	thunkEntry then collects the various arguments (thunk, stack pointer) in a VMCallbackContext and
	invokes the callback via invokeCallbackContext:."

	"thunk:		push   %esp				0x54							0xa1905454
	 thunk+01:	push   %esp				0x54
	 thunk+02:	nop   						0x90
	 thunk+03:	mov    $thunkEntry,%eax	0xb8 0x00 0x00 0x00 0x00	0x00000000 - entry
	 thunk+08:	nop   						0x90							0x68909090
	 thunk+09:	nop   						0x90
	 thunk+10:	nop   						0x90
	 thunk+11:	push   $thunk				0x68 0x00 0x00 0x00 0x00	0x00000000 - thunk
	 thunk+16:	call   *%eax					0xff 0xd0						0xc483d0ff
	 thunk+18:	add    $0xC,%esp			0x83 0xc4 0x0C				0x9090c30C
	 thunk+21:	ret							0xc3
	 thunk+22:	nop							0x90
	 thunk+23:	nop							0x90"
	self
		unsignedLongAt:  1 put: 16rB8905454;
		unsignedLongAt:  5 put: self primThunkEntryAddress;
		unsignedLongAt:  9 put: 16r68909090;
		unsignedLongAt: 13 put: self addressField;
		unsignedLongAt: 17 put: 16rC483D0FF;
		unsignedLongAt: 21 put: 16r9090C30C
]

{ #category : #'instance initialization' }
FFICallbackThunk >> initializeX86Stdcall: bytes [
	"Initialize the receiver with a __stdcall thunk with bytes argument bytes. (See initializeX86 fort more info)"
	"thunk:		push   %esp				0x54							0xa1905454
	 thunk+01:	push   %esp				0x54
	 thunk+02:	nop   						0x90
	 thunk+03:	mov    $thunkEntry,%eax	0xb8 0x00 0x00 0x00 0x00	0x00000000 - entry
	 thunk+08:	nop   						0x90							0x68909090
	 thunk+09:	nop   						0x90
	 thunk+10:	nop   						0x90
	 thunk+11:	push   $thunk				0x68 0x00 0x00 0x00 0x00	0x00000000 - thunk
	 thunk+16:	call   *%eax					0xff 0xd0						0xc483d0ff
	 thunk+18:	add    $0xC,%esp			0x83 0xc4 0x0C				0xBYTSc20C
	 thunk+21:	ret    $bytes				0xc2 0xBY 0xTS"

	self
		unsignedLongAt:  1 put: 16rB8905454;
		unsignedLongAt:  5 put: self primThunkEntryAddress;
		unsignedLongAt:  9 put: 16r68909090;
		unsignedLongAt: 13 put: self addressField;
		unsignedLongAt:  17 put: 16rC483D0FF;
		unsignedShortAt: 21 put: 16rC20C;
		unsignedShortAt: 23 put: bytes
]

{ #category : #'primitives-memory management' }
FFICallbackThunk >> primFree [
	self shouldNotImplement
]

{ #category : #'private-primitives' }
FFICallbackThunk >> primThunkEntryAddress [ "^<Integer>"
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
]
