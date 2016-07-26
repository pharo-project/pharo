tests
testZeroDuration
	| ts |
	ts := Timespan starting: Date today duration: Duration zero.
	self
		assert: ts start = ts end