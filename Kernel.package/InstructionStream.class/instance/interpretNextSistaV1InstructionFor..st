decoding
interpretNextSistaV1InstructionFor: client
	"Send to the argument, client, a message that specifies the next instruction."

	| byte div16 offset method extA extB savedPC |
	method := self method.
	"For a table of the bytecode set, see EncoderForSistaV1's class comment."
	"consume and compute any extensions first."
	extA := extB := 0.
	savedPC := pc.
	[byte := self method at: pc.
	 pc := pc + 1.
	 byte between: 16rE0 and: 16rE1] whileTrue:
		[| extByte |
		 extByte := self method at: pc.
		 pc := pc + 1.
		 byte = 16rE0
			ifTrue:
				[extA := (extA bitShift: 8) + extByte]
			ifFalse:
				[extB := (extB = 0 and: [extByte > 127])
							ifTrue: [extByte - 256]
							ifFalse: [(extB bitShift: 8) + extByte]]].
	div16 := byte // 16.
	offset := byte \\ 16.
	"We do an inline quasi-binary search on each of the possible 16 values of div16"
	div16 < 11 ifTrue:
		[div16 < 6 ifTrue:
			[div16 < 4 ifTrue:
				[div16 < 2 ifTrue:
					[div16 = 0 ifTrue:
						 [^client pushReceiverVariable: offset].
					^client pushLiteralVariable: (method literalAt: offset + 1)]. "div16 = 1"
				 ^client pushConstant: (method literalAt: byte \\ 32 + 1)].
			 div16 = 4 ifTrue:
				[offset < 12 ifTrue:
					[^client pushTemporaryVariable: offset].
				 offset = 12 ifTrue:
					[^client pushReceiver].
				 offset = 13 ifTrue:
					[^client pushConstant: true].
				 offset = 14 ifTrue:
					[^client pushConstant: false].
				 offset = 15 ifTrue:
					[^client pushConstant: nil]].
			"div16 = 5"
			 offset < 2 ifTrue:
				[^client pushConstant: offset].
			 offset = 2 ifTrue:
				[^self interpretSistaV1ExtendedPush: extB for: client].
			 offset = 3 ifTrue:
				[^client doDup].
			
			 offset = 8 ifTrue:
				[^client methodReturnReceiver].
			 offset = 9 ifTrue:
				[^client methodReturnConstant: true].
			 offset = 10 ifTrue:
				[^client methodReturnConstant: false].
			 offset = 11 ifTrue:
				[^client methodReturnConstant: nil].
			 offset = 12 ifTrue:
				[^client methodReturnTop].
			 offset = 13 ifTrue:
				[^client blockReturnConstant: nil].
			 offset = 14 ifTrue:
				[^client blockReturnTop].
			 offset = 15 ifTrue:
				[^client doNop].
			 ^self unusedBytecode: client at: savedPC].
		"short sends"
		div16 = 6 ifTrue:
			[^client
				send: (Smalltalk specialSelectorAt: offset + 1)
				super: false
				numArgs: (Smalltalk specialNargsAt: offset + 1)].
		 div16 = 7 ifTrue:
			[^client
				send: (Smalltalk specialSelectorAt: offset + 17)
				super: false
				numArgs: (Smalltalk specialNargsAt: offset + 17)].
		^client
			send: (method literalAt: offset + 1)
			super: false
			numArgs: div16 - 8].
	"div16 >= 11; bytecode >= 176"
	div16 < 14 ifTrue:
		[div16 = 11 ifTrue:
			[offset < 8 ifTrue:
				[^client jump: offset + 1].
			 ^client jump: offset - 7 if: true].
		 div16 = 12 ifTrue:
			[offset < 8 ifTrue:
				[^client jump: offset + 1 if: false].
			 ^client popIntoReceiverVariable: offset - 8].
		 "div16 = 13"
		 offset < 8 ifTrue:
		 	[^client popIntoTemporaryVariable: offset].
		 offset = 8 ifTrue: [ ^ client doPop ].
		 ^self unusedBytecode: client at: savedPC].
	"2 byte and 3 byte codes"
	byte < 248 ifTrue:
		[^self interpretNext2ByteSistaV1Instruction: byte for: client extA: extA extB: extB startPC: savedPC].
	^self interpretNext3ByteSistaV1Instruction: byte for: client extA: extA extB: extB startPC: savedPC