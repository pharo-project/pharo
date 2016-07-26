as yet unclassified
interpretV3ClosuresExtension: offset in: method for: client
	| type offset2 byte2 byte3 byte4 |
	offset <= 6 ifTrue: 
		["Extended op codes 128-134"
		byte2 := method at: pc. pc := pc + 1.
		offset <= 2 ifTrue:
			["128-130:  extended pushes and pops"
			type := byte2 // 64.
			offset2 := byte2 \\ 64.
			offset = 0 ifTrue: 
				[type = 0 ifTrue: [^client pushReceiverVariable: offset2].
				type = 1 ifTrue: [^client pushTemporaryVariable: offset2].
				type = 2  ifTrue: [^client pushConstant: (method literalAt: offset2 + 1)].
				type = 3 ifTrue: [^client pushLiteralVariable: (method literalAt: offset2 + 1)]].
			offset = 1 ifTrue: 
				[type = 0 ifTrue: [^client storeIntoReceiverVariable: offset2].
				type = 1 ifTrue: [^client storeIntoTemporaryVariable: offset2].
				type = 2 ifTrue: [self error: 'illegalStore'].
				type = 3 ifTrue: [^client storeIntoLiteralVariable: (method literalAt: offset2 + 1)]].
			offset = 2 ifTrue: 
				[type = 0 ifTrue: [^client popIntoReceiverVariable: offset2].
				type = 1 ifTrue: [^client popIntoTemporaryVariable: offset2].
				type = 2 ifTrue: [self error: 'illegalStore'].
				type = 3  ifTrue: [^client popIntoLiteralVariable: (method literalAt: offset2 + 1)]]].
		"131-134: extended sends"
		offset = 3 ifTrue:  "Single extended send"
			[^client send: (method literalAt: byte2 \\ 32 + 1)
					super: false numArgs: byte2 // 32].
		offset = 4 ifTrue:    "Double extended do-anything"
			[byte3 := method at: pc. pc := pc + 1.
			type := byte2 // 32.
			type = 0 ifTrue: [^client send: (method literalAt: byte3 + 1)
									super: false numArgs: byte2 \\ 32].
			type = 1 ifTrue: [^client send: (method literalAt: byte3 + 1)
									super: true numArgs: byte2 \\ 32].
			type = 2 ifTrue: [^client pushReceiverVariable: byte3].
			type = 3 ifTrue: [^client pushConstant: (method literalAt: byte3 + 1)].
			type = 4 ifTrue: [^client pushLiteralVariable: (method literalAt: byte3 + 1)].
			type = 5 ifTrue: [^client storeIntoReceiverVariable: byte3].
			type = 6 ifTrue: [^client popIntoReceiverVariable: byte3].
			type = 7 ifTrue: [^client storeIntoLiteralVariable: (method literalAt: byte3 + 1)]].
		offset = 5 ifTrue:  "Single extended send to super"
			[^client send: (method literalAt: byte2 \\ 32 + 1)
					super: true
					numArgs: byte2 // 32].
		offset = 6 ifTrue:   "Second extended send"
			[^client send: (method literalAt: byte2 \\ 64 + 1)
					super: false
					numArgs: byte2 // 64]].
	offset = 7 ifTrue: [^client doPop].
	offset = 8 ifTrue: [^client doDup].
	offset = 9 ifTrue: [^client pushActiveContext].
	byte2 := method at: pc. pc := pc + 1.
	offset = 10 ifTrue:
		[^byte2 < 128
			ifTrue: [client pushNewArrayOfSize: byte2]
			ifFalse: [client pushConsArrayWithElements: byte2 - 128]].
	byte3 := method at: pc.  pc := pc + 1.
	offset = 11 ifTrue: [^client callPrimitive: byte2 + (byte3 bitShift: 8)].
	offset = 12 ifTrue: [^client pushRemoteTemp: byte2 inVectorAt: byte3].
	offset = 13 ifTrue: [^client storeIntoRemoteTemp: byte2 inVectorAt: byte3].
	offset = 14 ifTrue: [^client popIntoRemoteTemp: byte2 inVectorAt: byte3].
	"offset = 15"
	byte4 := method at: pc.  pc := pc + 1.
	^client
		pushClosureCopyNumCopiedValues: (byte2 bitShift: -4)
		numArgs: (byte2 bitAnd: 16rF)
		blockSize: (byte3 * 256) + byte4