tests - properties
testIsReadInMethod
	
	ivarForTesting printString.
	self assert: ((self class slotNamed: #ivarForTesting) isReadIn: self class >> testSelector)