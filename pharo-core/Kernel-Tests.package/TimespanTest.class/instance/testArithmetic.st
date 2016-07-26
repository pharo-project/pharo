tests
testArithmetic

	| ts1 ts2 d |
	ts1 := timespan + 2 days.
	ts2 := ts1 - 2 days.
	d := ts1 - (DateAndTime year: 2003 month: 03 day: 20).

	self 
		assert: (ts1 start = 
				 (DateAndTime year: 2003 month: 03 day: 24 hour: 12 minute: 0 second: 0));
		assert: (ts1 duration = timespan duration);
		assert: (ts2 start = timespan start);
		assert: (ts2 duration = timespan duration).

	self
		assert: d = (Duration days: 4 hours: 12 minutes: 0 seconds: 0)

