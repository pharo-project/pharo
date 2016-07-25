tests
testByWeekNumberInCurrentYear
	week := Week week: 2.
	self assert: week year asYear equals: Year current.