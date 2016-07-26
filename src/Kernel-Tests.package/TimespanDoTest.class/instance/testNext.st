tests
testNext
	self assert: aTimespan next 
			= (Timespan
					starting: (DateAndTime
							year: 2003
							month: 4
							day: 8
							hour: 0
							minute: 0
							second: 0)
					duration: aDuration)