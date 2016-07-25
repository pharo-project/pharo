tests
testTicks
	self assert: aDateAndTime ticks =  (DateAndTime julianDayNumber: 2415386) ticks.
	self assert: aDateAndTime ticks = #(2415386 0 0)