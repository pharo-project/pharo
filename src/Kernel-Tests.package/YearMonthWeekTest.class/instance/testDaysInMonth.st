tests
testDaysInMonth
	self assert: (Month daysInMonth: 2 forYear: 2000) = 29.
	self assert: (Month daysInMonth: 2 forYear: 2001) = 28.
	self assert: (Month  daysInMonth: 2 forYear: 2004) = 29.
	self assert: (Month  daysInMonth: 2 forYear: 2100) = 28.
	
	self assert: (Month  daysInMonth: 'January' forYear: 2003) = 31.
	self assert: (Month  daysInMonth: 'February' forYear: 2003) = 28.
	self assert: (Month  daysInMonth: 'March' forYear: 2003) = 31.
	self assert: (Month  daysInMonth: 'April' forYear: 2003) = 30.
	self assert: (Month  daysInMonth: 'May' forYear: 2003) = 31.
	self assert: (Month  daysInMonth: 'June' forYear: 2003) = 30.
	self assert: (Month  daysInMonth: 'July' forYear: 2003) = 31.
	self assert: (Month  daysInMonth: 'August' forYear: 2003) = 31.
	self assert: (Month  daysInMonth: 'September' forYear: 2003) = 30.
	self assert: (Month  daysInMonth: 'October' forYear: 2003) = 31.
	self assert: (Month  daysInMonth: 'November' forYear: 2003) = 30.
	self assert: (Month  daysInMonth: 'December' forYear: 2003) = 31.