tests
testTicks
	self assert: aDateAndTime ticks =  (DateAndTime julianDayNumber: 2444240) ticks.
	self assert: aDateAndTime ticks = #(2444240 0 0)