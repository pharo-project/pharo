tests - bitLogic
testBitShift

	self assert: 2r11 << 2 equals: 2r1100.
	self assert: (2r11 bitShift: 2) equals: 2r1100.
	self assert: 2r1011 >> 2 equals: 2r10.
	self assert: (2r1011 bitShift: -2) equals: 2r10.