tests - bitLogic
testBitMask

	self assert: (2r11 allMask: 2r11).
	self deny: (2r10 allMask: 2r11).
	self deny: (2r01 allMask: 2r11).
	
	self assert: (2r10 anyMask: 2r11).
	self assert: (2r01 anyMask: 2r11).
	self deny: (2r00 anyMask: 2r11).