tests
testAccessing

	self 
		assert: (timespan start =
				 (DateAndTime year: 2003 month: 03 day: 22 hour: 12 minute: 0 second: 0));
		assert: timespan duration = (Duration hours: 100);
		assert: timespan month = 3;
		assert: timespan monthName = 'March';
		assert: timespan monthAbbreviation = 'Mar'
		

