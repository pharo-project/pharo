tests
testMonth
	self assert: aDateAndTime month  = 1.
	self assert: aDateAndTime monthAbbreviation = 'Jan'.
	self assert: aDateAndTime monthName = 'January'.
	self assert: aDateAndTime monthIndex = 1.