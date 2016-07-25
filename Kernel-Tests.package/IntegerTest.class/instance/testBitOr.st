tests - bitLogic
testBitOr

	self assert: (2r0101 | 2r1010) equals: 2r1111.
	self assert: (2r0101 bitOr: 2r1010) equals: 2r1111.