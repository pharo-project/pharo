tests
testDayMonthYearDo
	
	january23rd2004 dayMonthYearDo: [ :day :month :year | 
		self assert: day equals: 23.
		self assert: month equals: 1.
		self assert: year equals: 2004 ].