tests
testInquiries

	self	
		assert: june2nd1973 dayOfMonth equals: 2;
		assert: june2nd1973 dayOfYear equals: 153;
		assert: june2nd1973 daysInMonth equals: 30;
		assert: june2nd1973 daysInYear equals: 365;
		assert: june2nd1973 daysLeftInYear equals: (365 - 153);
		assert: june2nd1973 firstDayOfMonth equals: 152.
