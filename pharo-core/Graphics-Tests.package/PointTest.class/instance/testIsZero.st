tests - testing
testIsZero

	self assert: (0@0) isZero.	
	self deny:  (0@1) isZero.
	self deny:  (1@0) isZero.
	self deny:  (1@1) isZero.