tests
testAsYear

	self 
		assert: 1 year days = 365;
		assert: 0.5 year asHours = ((364 / 2) * 24 + 12)