tests - bitLogic
testBitClear

	self assert: (2r1111 bitClear: 2r1000) equals: 2r0111.
	self assert: (2r1111 bitClear: 2r0100) equals: 2r1011.
	self assert: (2r1111 bitClear: 2r0010) equals: 2r1101.
	self assert: (2r1111 bitClear: 2r0001) equals: 2r1110.