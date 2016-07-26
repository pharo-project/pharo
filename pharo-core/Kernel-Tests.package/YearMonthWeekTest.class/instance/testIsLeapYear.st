tests
testIsLeapYear
	self assert: (Year isLeapYear: 2000).
	self deny: (Year isLeapYear: 2001).
	self assert: (Year isLeapYear: 2004).
	self deny: (Year isLeapYear: 2100).
	self deny: (Year isLeapYear: 2002).