tests - 32 bit
test32BitTranslucentBlackIsBlack
	|form|
	form := Form extent: 1@1 depth: 32.
	form colorAt: 0@0 put: (Color black alpha: 0.0).
	self assert:  0 equals: (form pixelValueAt: 0@0)