tests
testTicks
	self 
		assert: aDateAndTime ticks  
		equals: ((DateAndTime julianDayNumber: 2453065) + 41580 seconds) ticks.
	self 
		assert: aDateAndTime ticks 
		equals:  #(2453065 41580 0)