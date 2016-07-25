decoding
interpretNextV3PlusClosureInstructionFor: client 
	"Send to the argument, client, a message that specifies the type of the 
	next instruction."

	| byte type offset method |
	method := self method.  
	byte := method at: pc.
	type := byte // 16.  
	offset := byte \\ 16.   
	pc := pc+1.
	"We do an inline binary search on each of the possible 16 values of type:
	The old, cleaner but slowe code is retained as a comment below"
	type < 8
	ifTrue: [type < 4
				ifTrue: [type < 2
						ifTrue: [type < 1
								ifTrue: ["type = 0"
									^ client pushReceiverVariable: offset]
								ifFalse: ["type = 1"
									^ client pushTemporaryVariable: offset]]
						ifFalse: [type < 3
								ifTrue: ["type = 2"
									^ client
										pushConstant: (method literalAt: offset + 1)]
								ifFalse: ["type = 3"
									^ client
										pushConstant: (method literalAt: offset + 17)]]]
				ifFalse: [type < 6
						ifTrue: [type < 5
								ifTrue: ["type = 4"
									^ client
										pushLiteralVariable: (method literalAt: offset + 1)]
								ifFalse: ["type = 5"
									^ client
										pushLiteralVariable: (method literalAt: offset + 17)]]
						ifFalse: [type < 7
								ifTrue: ["type = 6"
									offset < 8
										ifTrue: [^ client popIntoReceiverVariable: offset]
										ifFalse: [^ client popIntoTemporaryVariable: offset - 8]]
								ifFalse: ["type = 7"
									offset = 0
										ifTrue: [^ client pushReceiver].
									offset < 8
										ifTrue: [^ client
												pushConstant: (SpecialConstants at: offset)].
									offset = 8
										ifTrue: [^ client methodReturnReceiver].
									offset < 12
										ifTrue: [^ client
												methodReturnConstant: (SpecialConstants at: offset - 8)].
									offset = 12
										ifTrue: [^ client methodReturnTop].
									offset = 13
										ifTrue: [^ client blockReturnTop].
									offset > 13
										ifTrue: [^ self unusedBytecode: client at: pc - 1 ]]]]]
		ifFalse: [type < 12
				ifTrue: [type < 10
						ifTrue: [type < 9
								ifTrue: ["type = 8"
									^ self
										interpretV3PlusClosureExtension: offset 
										in: method 
										for: client]
								ifFalse: ["type = 9 (short jumps)"
									offset < 8
										ifTrue: [^ client jump: offset + 1].
									^ client jump: offset - 8 + 1 if: false]]
						ifFalse: [type < 11
								ifTrue: ["type = 10 (long jumps)"
									byte := method at: pc.
									pc := pc + 1.
									offset < 8
										ifTrue: [^ client jump: offset - 4 * 256 + byte].
									^ client jump: (offset bitAnd: 3)
											* 256 + byte if: offset < 12]
								ifFalse: ["type = 11"
									^ client
										send: (Smalltalk specialSelectorAt: offset + 1)
										super: false
										numArgs: (Smalltalk specialNargsAt: offset + 1)]]]
				ifFalse: [type = 12
						ifTrue: [^ client
								send: (Smalltalk specialSelectorAt: offset + 17)
								super: false
								numArgs: (Smalltalk specialNargsAt: offset + 17)]
						ifFalse: ["type = 13, 14 or 15"
							^ client
								send: (method literalAt: offset + 1)
								super: false
								numArgs: type - 13]]].


"    old code 
	type=0 ifTrue: [^client pushReceiverVariable: offset].
	type=1 ifTrue: [^client pushTemporaryVariable: offset].
	type=2 ifTrue: [^client pushConstant: (method literalAt: offset+1)].
	type=3 ifTrue: [^client pushConstant: (method literalAt: offset+17)].
	type=4 ifTrue: [^client pushLiteralVariable: (method literalAt: offset+1)].
	type=5 ifTrue: [^client pushLiteralVariable: (method literalAt: offset+17)].
	type=6 
		ifTrue: [offset<8
					ifTrue: [^client popIntoReceiverVariable: offset]
					ifFalse: [^client popIntoTemporaryVariable: offset-8]].
	type=7
		ifTrue: [offset=0 ifTrue: [^client pushReceiver].
				offset<8 ifTrue: [^client pushConstant: (SpecialConstants at: offset)].
				offset=8 ifTrue: [^client methodReturnReceiver].
				offset<12 ifTrue: [^client methodReturnConstant: 
												(SpecialConstants at: offset-8)].
				offset=12 ifTrue: [^client methodReturnTop].
				offset=13 ifTrue: [^client blockReturnTop].
				offset>13 ifTrue: [^self error: 'unusedBytecode']].
	type=8 ifTrue: [^self interpretExtension: offset in: method for: client].
	type=9
		ifTrue:  short jumps
			[offset<8 ifTrue: [^client jump: offset+1].
			^client jump: offset-8+1 if: false].
	type=10 
		ifTrue:  long jumps
			[byte:= method at: pc.  pc:= pc+1.
			offset<8 ifTrue: [^client jump: offset-4*256 + byte].
			^client jump: (offset bitAnd: 3)*256 + byte if: offset<12].
	type=11 
		ifTrue: 
			[^client 
				send: (Smalltalk specialSelectorAt: offset+1) 
				super: false
				numArgs: (Smalltalk specialNargsAt: offset+1)].
	type=12 
		ifTrue: 
			[^client 
				send: (Smalltalk specialSelectorAt: offset+17) 
				super: false
				numArgs: (Smalltalk specialNargsAt: offset+17)].
	type>12
		ifTrue: 
			[^client send: (method literalAt: offset+1) 
					super: false
					numArgs: type-13]"