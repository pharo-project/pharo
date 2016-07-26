tests
testAsMonth
	self
		assert: aDateAndTime asMonth
		equals: (Month year: 2004 month: 'February').
