tests
testPrevious
	self assert: aTimespan  = aDisjointTimespan previous.
	self assert: aTimespan next previous = aTimespan 
