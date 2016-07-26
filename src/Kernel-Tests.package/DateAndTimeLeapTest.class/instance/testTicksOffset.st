tests
testTicksOffset
	self assert: aDateAndTime =  (aDateAndTime ticks:  #(2453065 48780 0) offset: DateAndTime localOffset).

