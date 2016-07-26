tests
testIndexOfMonth

	self assert: (Date indexOfMonth: #January) equals: 1.	
	self assert: (Date indexOfMonth: #December) equals: 12.
	
	self should: [ Date indexOfMonth: #NonExistantMonth ] raise: Error.