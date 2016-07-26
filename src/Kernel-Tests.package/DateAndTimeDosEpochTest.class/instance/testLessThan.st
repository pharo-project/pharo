tests
testLessThan
	self assert: aDateAndTime  < (aDateAndTime + '1:00:00:00').
	self assert: aDateAndTime + -1 < aDateAndTime.
	