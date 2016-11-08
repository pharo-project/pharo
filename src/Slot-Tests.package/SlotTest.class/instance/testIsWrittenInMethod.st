testing - properties
testIsWrittenInMethod
	
	ivarForTesting := #test.
	self assert: ((self class slotNamed: #ivarForTesting) isWrittenIn: self class >> testSelector)