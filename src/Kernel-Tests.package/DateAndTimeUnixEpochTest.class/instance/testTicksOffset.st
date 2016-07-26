tests
testTicksOffset
	self assert: aDateAndTime =  (aDateAndTime ticks:  #(2415386 0 0) offset: DateAndTime localOffset).
