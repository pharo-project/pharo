tests
testNameOfMonth

	self assert: (Date nameOfMonth: 1) equals: #January.	
	self assert: (Date nameOfMonth: 12) equals: #December.

	self should: [ Date nameOfMonth: 0 ] raise: SubscriptOutOfBounds.
	self should: [ Date nameOfMonth: 13 ] raise: SubscriptOutOfBounds.
