tests - zero behavior
testIsZero
	self assert: 0.0 isZero.
	self deny:  0.1 isZero.