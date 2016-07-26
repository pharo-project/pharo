tests
testMiddleOf
	self assert: (aDateAndTime middleOf: aDuration)  = 
	 (Timespan starting: (DateAndTime year: 2004 month: 2 day: 29 hour: 6 minute: 46 second: 30 offset: 2 hours)
	duration: (Duration days: 0 hours: 13 minutes: 33 seconds: 0 nanoSeconds: 0 ))
	