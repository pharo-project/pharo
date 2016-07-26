tests
testCurrent
	self assert: (Timespan starting: DateAndTime current)
			<= Timespan current.
	self assert:  Timespan current
			<= (Timespan starting: DateAndTime current)