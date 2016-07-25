tests
testDaysInYear

	self assert: (Date daysInYear: 2000) equals: 366.	
	self assert: (Date daysInYear: 2008) equals: 366.	
	self assert: (Date daysInYear: 2100) equals: 365	
