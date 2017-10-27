instruction stream support
selectorToSendOrItselfFor: anInstructionStream in: method at: pc
	"If anInstructionStream is at a send bytecode then answer the send's selector,
	 otherwise answer anInstructionStream itself.  The rationale for answering
	 anInstructionStream instead of, say, nil, is that potentially any existing object
	 can be used as a selector, but since anInstructionStream postdates the method,
	 it can't be one of them.

	 The complication is that for convenience we allow the pc to point to the
	 raw send bytecode after its extension(s), or at the extension(s) preceeding it.
	96-111		0110 iiii			Send Arithmetic Message #iiii (+ - < > <= >= = ~= * / \\ @ bitShift: // bitAnd: bitOr:)
	112-119	01110 iii			Send Special Message #iii + 0 (at: at:put: size next nextPut: atEnd == class)
	120-127	01111 iii			Send Special Message #iii + 8 (~~ value value: do: new new: x y)
	128-143	1000 iiii			Send Literal Selector #iiii With 0 Argument
	144-159	1001 iiii			Send Literal Selector #iiii With 1 Arguments
	160-175	1010 iiii			Send Literal Selector #iiii With 2 Arguments
	*	224	11100000	aaaaaaaa	Extend A (Ext A = Ext A prev * 256 + Ext A)
	*	225	11100001	bbbbbbbb	Extend B (Ext B = Ext B prev * 256 + Ext B)
	**	234	11101010	iiiiijjj		Send Literal Selector #iiiii (+ Extend A * 32) with jjj (+ Extend B * 8) Arguments
	**	235	11101011	iiiiijjj	ExtendB < 64
										ifTrue: [Send To Superclass Literal Selector #iiiii (+ Extend A * 32) with jjj (+ Extend B * 8) Arguments]
										ifFalse: [Send To Superclass of Stacked Class Literal Selector #iiiii (+ Extend A * 32) with jjj (+ (Extend B "

	| byte |
	byte := method at: pc.
	byte < 96 ifTrue:
		[^anInstructionStream].
	byte <= 175 ifTrue: 
		["special byte or short send"
		 ^byte >= 128
			ifTrue: [method literalAt: (byte bitAnd: 15) + 1]
			ifFalse: [Smalltalk specialSelectorAt: byte - 95]].
	byte < 234 ifTrue: "need to check for either extension cuz order of extensions is not restricted. so extB could preceed extA"
		[(byte >= 224 and: [byte <= 225]) ifTrue:
			[^self extensionsAt: pc in: method into:
				[:extA :extB :nExtBytes| | byteAfter index |
				byteAfter := method at: pc + nExtBytes.
				(byteAfter >= 234 and: [byteAfter <= 235])
					ifTrue:
						[index := ((method at: pc + nExtBytes + 1) bitShift: -3) + (extA bitShift: 5).
						 method literalAt: index + 1]
					ifFalse: [anInstructionStream]]].
		^anInstructionStream].
	byte > 235 ifTrue:
		[^anInstructionStream].
	"they could be extended..."
	^self extensionsFor: pc in: method into:
		[:extA :extB :nExtBytes| | index |
		 index := ((method at: pc + 1) bitShift: -3) + (extA bitShift: 5).
		 method literalAt: index + 1]