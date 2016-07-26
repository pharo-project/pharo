tests
testMonth
	self assert: aDateAndTime month  = 2.
	self assert: aDateAndTime monthAbbreviation = 'Feb'.
	self assert: aDateAndTime monthName = 'February'.
	self assert: aDateAndTime monthIndex = 2.