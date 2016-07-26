tests
testAsMonth
	self
		assert: aDateAndTime asMonth 
		equals: (Month year: 1980 month: 'January'). 
