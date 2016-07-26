tests
testAsMonth
	self 
		assert: aDateAndTime asMonth
		equals: (Month year: 1901 month: 'January'). 
