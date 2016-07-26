testing
testNewPackage
	| newPackage |
	
	newPackage:= RGPackage named: 'Collections-Sequenceable'.
	
	self assert: (newPackage isPackage).
	self assert: (newPackage name = 'Collections-Sequenceable').
	self assert: (newPackage classes isEmpty).
	self assert: (newPackage traits isEmpty).
	self assert: (newPackage packages isEmpty).
	self assert: (newPackage package isNil).
	self assert: (newPackage parent == Smalltalk globals).