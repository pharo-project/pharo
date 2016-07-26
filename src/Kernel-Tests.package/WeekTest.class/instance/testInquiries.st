tests
testInquiries

	self
		assert: week start asDate = '28 June 1998' asDate;
		assert: week end asDate = '4 July 1998' asDate;
		assert: week index = 5;
		assert: week duration = (7 days).
