bytecode generation
genPushInteger: anInteger
	"80			01010000				Push 0
	 81			01010001				Push 1
	 232		11101000	i i i i i i i i	Push Integer #iiiiiiii (+ Extend B * 256, where bbbbbbbb = sddddddd, e.g. -32768 = i=0, a=0, s=1)"
	"Why restrict the range to 16 bits when we could encode arbitrarily large integers?
	 Well, 16 bits requires 4 bytes (extB + byte, 78 + byte) and so beyond this range we lose space
	 verses a single-byte pushLiteral and a 4 byte integer literal on 32-bits.  And generating the same
	 bytecode on 64-bit and 32-bit is important if we want to be able to load binary code from one to
	 the other (e.g. via Fuel)."
	anInteger = 0 ifTrue:
		[stream nextPut: 80.
		 ^self].
	anInteger = 1 ifTrue:
		[stream nextPut: 81.
		 ^self].
	(anInteger < -32768 or: [anInteger > 32767]) ifTrue:
		[^self outOfRangeError: 'integer' index: anInteger range: -32768 to: 32767].
	(anInteger < 0 or: [anInteger > 255]) ifTrue:
		[self genSignedSingleExtendB: (anInteger bitShift: -8)].
	stream
		nextPut: 232;
		nextPut: (anInteger bitAnd: 255)