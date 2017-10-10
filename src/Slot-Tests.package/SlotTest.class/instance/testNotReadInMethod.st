tests - properties
testNotReadInMethod
	
	self deny: ((self class slotNamed: #ivarForTesting) isReadIn: self class >> testSelector)