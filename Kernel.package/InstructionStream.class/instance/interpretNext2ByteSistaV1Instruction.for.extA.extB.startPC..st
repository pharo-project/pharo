decoding
interpretNext2ByteSistaV1Instruction: bytecode for: client extA: extA extB: extB startPC: startPC
	"Send to the argument, client, a message that specifies the next instruction.
	 This method handles the two-byte codes.
	 For a table of the bytecode set, see EncoderForV1's class comment."

	| byte method |
	method := self method.
	byte := self method at: pc.
	pc := pc + 1.
	"We do an inline quasi-binary search on bytecode"
	bytecode < 234 ifTrue: "pushes"
		[bytecode < 231 ifTrue:
			[bytecode < 229 ifTrue:
				[| literal |
				 bytecode = 226 ifTrue:
					[^client pushReceiverVariable: (extA bitShift: 8) + byte].
				 literal := method literalAt: (extA bitShift: 8) + byte + 1.
				 bytecode = 227 ifTrue:
					[^client pushLiteralVariable: literal].
				 ^client pushConstant: literal].
			bytecode = 229 ifTrue:
				[^client pushTemporaryVariable: byte]. 
			^client pushClosureTemps: byte]. 
		bytecode = 231 ifTrue:
			[^byte < 128
				ifTrue: [client pushNewArrayOfSize: byte]
				ifFalse: [client pushConsArrayWithElements: byte - 128]].
		bytecode = 232 ifTrue:
			[^client pushConstant: (extB bitShift: 8) + byte].
		^client pushConstant: (Character value: (extB bitShift: 8) + byte)].
	bytecode < 240 ifTrue: "sends, trap and jump"
		[bytecode < 236 ifTrue: "sends"
			[^client
				send: (method literalAt: (extA bitShift: 5) + (byte // 8) + 1)
				super: bytecode = 235
				numArgs: (extB bitShift: 3) + (byte \\ 8)].
		 bytecode = 236 ifTrue:
			[^client trapIfNotInstanceOf: (method literalAt: (extA bitShift: 8) + byte + 1)].
		bytecode = 237 ifTrue:
			[^client jump: (extB bitShift: 8) + byte].
		 ^client jump: (extB bitShift: 8) + byte if: bytecode = 238].
	bytecode < 243 ifTrue:
		[bytecode = 240 ifTrue:
			[^client popIntoReceiverVariable: (extA bitShift: 8) + byte].
		 bytecode = 241 ifTrue:
			[^client popIntoLiteralVariable: (method literalAt: (extA bitShift: 8) + byte + 1)].
		 ^client popIntoTemporaryVariable: byte].
	bytecode = 243 ifTrue:
		[^client storeIntoReceiverVariable: (extA bitShift: 8) + byte].
	bytecode = 244 ifTrue:
		[^client storeIntoLiteralVariable: (method literalAt: (extA bitShift: 8) + byte + 1)].
	bytecode = 245 ifTrue:
		[^client storeIntoTemporaryVariable: byte].
	"246-247	1111011 i	xxxxxxxx	UNASSIGNED"
	^self unusedBytecode: client at: startPC