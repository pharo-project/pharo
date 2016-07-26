bytecode generation
genJumpLong: distance
	"237		11101101	iiiiiiii		Jump #iiiiiiii (+ Extend B * 256, where bbbbbbbb = sddddddd, e.g. -32768 = i=0, a=0, s=1)"
	(distance between: -32768 and: 32767) ifFalse:
		[^self outOfRangeError: 'index' index: distance range: -32768 to: 32767].
	(distance < 0 or: [distance > 255]) ifTrue:
		[self genSignedSingleExtendB: (distance bitShift: -8)].
	stream
		nextPut: 237;
		nextPut: (distance bitAnd: 255)