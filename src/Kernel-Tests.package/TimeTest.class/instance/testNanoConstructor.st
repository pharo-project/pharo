tests - input
testNanoConstructor

	| timeFromString timeFromNano timeFromNanoSecond |
	
	timeFromString := Time fromString: '01:23:45.67809'.
	timeFromNano := Time hour: 1 minute: 23 second: 45 nano: 67809.
	timeFromNanoSecond := Time hour: 1 minute: 23 second: 45 nanoSecond: 67809.
	
	self 
		assert: timeFromString equals: timeFromNano;
		deny: timeFromNano = timeFromNanoSecond.
	
	timeFromString := Time fromString: '01:23:45.0'.
	timeFromNano := Time hour: 1 minute: 23 second: 45 nano: 0.
	
	self assert: timeFromString equals: timeFromNano.

	timeFromString := Time fromString: '01:23:45.1234567890'.
	timeFromNano := Time hour: 1 minute: 23 second: 45 nano: 1234567890.
	
	self assert: timeFromString equals: timeFromNano
