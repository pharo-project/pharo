instance initalization
initializeARM32
	"Initialize the receiver with a __ccall thunk."
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