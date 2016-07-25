tests
testDayMonthYearDo
	self assert: (aDateAndTime dayMonthYearDo: [:eachDay :eachMonth :eachYear |  eachYear])  = 2004.
	self assert: (aDateAndTime dayMonthYearDo: [:eachDay :eachMonth :eachYear |  eachMonth]) = 2.
	self assert: (aDateAndTime dayMonthYearDo: [:eachDay :eachMonth :eachYear |  eachDay]) = 29.
