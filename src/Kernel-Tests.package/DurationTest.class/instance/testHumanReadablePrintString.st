tests
testHumanReadablePrintString
	self assert: 0 seconds humanReadablePrintString equals: '0 seconds'.
	self assert: 1 second humanReadablePrintString equals: '1 second'.
	self assert: 1 second negated humanReadablePrintString equals: '- 1 second'.
	self 
		assert: 1002003004 nanoSeconds humanReadablePrintString 
		equals: '1 second 2 milliseconds 3 microseconds 4 nanoseconds'.
	self 
		assert: (Duration days: 1 hours: 1 minutes: 1 seconds: 1) humanReadablePrintString 
		equals: '1 day 1 hour 1 minute 1 second'.
	self 
		assert: (Duration days: 2 hours: 4 minutes: 8 seconds: 16) humanReadablePrintString 
		equals: '2 days 4 hours 8 minutes 16 seconds'