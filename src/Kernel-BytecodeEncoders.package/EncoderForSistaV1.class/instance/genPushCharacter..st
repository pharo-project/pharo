bytecode generation
genPushCharacter: aCharacterOrCode
	"233		11101001	i i i i i i i i	Push Character #iiiiiiii (+ Extend B * 256)"
	"Why restrict the range to 16 bits when we could encode arbitrarily 32-bit Characters?
	 Well, 16 bits requires 4 bytes (extB + byte, 78 + byte) and so beyond this range we
	 lose space verses a single-byte pushLiteral and a 4 byte Character literal on 32-bits.
	 And generating the same bytecode on 64-bit and 32-bit is important if we want to be
	 able to load binary code from one to the other (e.g. via Fuel)."
	| code |
	code := aCharacterOrCode isInteger ifTrue: [aCharacterOrCode] ifFalse: [aCharacterOrCode asInteger].
	(code < 0 or: [code > 65535]) ifTrue:
		[^self outOfRangeError: 'character' index: code range: 0 to: 65535].
	(code > 255) ifTrue:
		[self genUnsignedSingleExtendB: (code bitShift: -8)].
	stream
		nextPut: 233;
		nextPut: (code bitAnd: 255)