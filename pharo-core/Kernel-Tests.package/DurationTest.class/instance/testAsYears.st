tests
testAsYears

	self 
		assert: 2 years days = 730;
		assert:	0.5 year asHours = ((364 / 2) * 24 + 12)