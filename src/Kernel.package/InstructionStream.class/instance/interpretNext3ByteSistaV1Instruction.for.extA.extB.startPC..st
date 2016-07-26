decoding
interpretNext3ByteSistaV1Instruction: bytecode for: client extA: extA extB: extB startPC: startPC
	"Send to the argument, client, a message that specifies the next instruction.
	 This method handles the three-byte codes.
	 For a table of the bytecode set, see EncoderForSistaV1's class comment."

	| method byte2 byte3 |
	method := self method.
	byte2 := method at: pc.
	byte3 := method at: pc + 1.
	pc := pc + 2.
	"we search the bytecodes by what we expect to be the static frequency."
	bytecode = 248 ifTrue:
		[^client callPrimitive: byte2 + (byte3 bitShift: 8)].
	bytecode = 250 ifTrue:
		["**	250  11111010  eeiiikkk  jjjjjjjj  Push Closure Num Copied iii (+ExtA//16*8) Num Args kkk (+ ExtA\\16*8) BlockSize jjjjjjjj (+ExtB*256). ee = num extensions"
		 ^client
			pushClosureCopyNumCopiedValues: ((byte2 bitShift: -3) bitAnd: 7) + (extA // 16 bitShift: 3)
			numArgs: (byte2 bitAnd: 7) + (extA \\ 16 bitShift: 3)
			blockSize: byte3 + (extB bitShift: 8)].
	bytecode = 251 ifTrue:
		[^client pushRemoteTemp: byte2 inVectorAt: byte3].
	bytecode = 252 ifTrue:
		[^client storeIntoRemoteTemp: byte2 inVectorAt: byte3].
	bytecode = 253 ifTrue:
		[^client popIntoRemoteTemp: byte2 inVectorAt: byte3].
	"249		11111001 	xxxxxxxx	syyyyyyy	Reserved for Push Float"
	"254-255	1111111 i	xxxxxxxx	yyyyyyyy	UNASSIGNED"
	^self unusedBytecode: client at: startPC