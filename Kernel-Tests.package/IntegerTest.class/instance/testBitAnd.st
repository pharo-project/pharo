tests - bitLogic
testBitAnd

	self assert: (2r1100 bitAnd: 2r1010) equals: 2r1000.
	self assert: (2r1100 & 2r1010) equals: 2r1000.
	self assert: (-2 bitAnd: 16rFFFFFFFF) equals: 16rFFFFFFFE.
	self assert: (-2 & 16rFFFFFFFF) equals: 16rFFFFFFFE.