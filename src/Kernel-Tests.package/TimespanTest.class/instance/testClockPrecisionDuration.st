tests
testClockPrecisionDuration
	| ts |
	ts := Timespan starting: Date today duration: DateAndTime clockPrecision.
	self
		assert: ts start = ts end