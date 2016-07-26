tests
testAsMonth
	self 
		assert: aDateAndTime asMonth
		equals: (Month year: 1970 month: 'January'). 
