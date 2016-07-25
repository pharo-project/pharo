tests
testDayMonthYearDo
	|iterations|
	iterations := 0.
	self assert: (aDateAndTime dayMonthYearDo: [:eachDay :eachMonth :eachYear |  iterations := iterations + 1])  = 1.
	self assert: (aDateAndTime dayMonthYearDo: [:eachDay :eachMonth :eachYear |  eachYear])  = 1970.
	self assert: (aDateAndTime dayMonthYearDo: [:eachDay :eachMonth :eachYear |  eachMonth]) = 1.
	self assert: (aDateAndTime dayMonthYearDo: [:eachDay :eachMonth :eachYear |  eachDay]) = 1.
