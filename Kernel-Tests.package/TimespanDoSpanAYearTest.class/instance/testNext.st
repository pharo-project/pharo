tests
testNext

	self assert: aTimespan next
			= (Timespan
					starting: (DateAndTime
							year: 2005
							month: 3
							day: 26
							hour: 0
							minute: 0
							second: 0)
					duration: aDuration)