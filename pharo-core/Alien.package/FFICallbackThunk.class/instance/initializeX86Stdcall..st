instance initalization
initializeX86Stdcall: bytes
	"Initialize the receiver with a __stdcall thunk with bytes argument bytes."
	"thunk:		push   %esp					0x54							0xa1905454
	 thunk+01:	push   %esp					0x54
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