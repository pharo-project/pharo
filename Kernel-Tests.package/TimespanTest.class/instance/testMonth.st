tests
testMonth
	self assert: aTimespan month  = 1.
	self assert: aTimespan monthName = 'January'.
	self assert: aTimespan monthIndex = 1.