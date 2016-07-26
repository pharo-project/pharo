tests
testTicksOffset
	self assert: aDateAndTime =  (aDateAndTime ticks:  #(2444240 0 0) offset: DateAndTime localOffset).
