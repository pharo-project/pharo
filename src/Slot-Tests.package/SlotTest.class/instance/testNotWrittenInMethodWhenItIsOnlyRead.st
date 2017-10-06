tests - properties
testNotWrittenInMethodWhenItIsOnlyRead
	
	ivarForTesting printString.
	self deny: ((self class slotNamed: #ivarForTesting) isWrittenIn: self class >> testSelector)