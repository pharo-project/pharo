tests
testDaysInYear
	self assert: (Year daysInYear: 2000) = 366.
	self assert: (Year daysInYear: 2001) = 365.
	self assert: (Year daysInYear: 2004) = 366.
	self assert: (Year daysInYear: 2100) = 365.
	self assert: (Year daysInYear: 2003) = 365.