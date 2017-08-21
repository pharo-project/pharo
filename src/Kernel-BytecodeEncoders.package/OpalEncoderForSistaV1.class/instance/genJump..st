bytecode generation
genJump: distance
	(distance > 0 and: [distance < 9]) ifTrue:
		["176-183	10110 iii			Jump iii + 1 (i.e., 1 through 8)"
		 stream nextPut: 175 + distance.
		 ^self].
	"237		11101101	iiiiiiii		Jump #iiiiiiii (+ Extend B * 256, where bbbbbbbb = sddddddd, e.g. -32768 = i=0, a=0, s=1)"
	^self genJumpLong: distance