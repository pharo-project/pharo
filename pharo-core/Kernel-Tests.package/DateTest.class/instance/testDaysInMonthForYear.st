tests
testDaysInMonthForYear

	self assert: (Date daysInMonth: #February forYear: 2008) equals: 29.	
	self assert: (Date daysInMonth: #February forYear: 2000) equals: 29.	
	self assert: (Date daysInMonth: #February forYear: 2100) equals: 28.	
	self assert: (Date daysInMonth: #July forYear: 2100) equals: 31.	