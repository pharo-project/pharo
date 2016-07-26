tests
testFirstDayOfMonth
	self deny: aDateAndTime firstDayOfMonth =  1.
	self assert: aDateAndTime firstDayOfMonth = 32
