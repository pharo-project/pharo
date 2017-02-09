tests
testTime
	| time |
	time := Time hour: 6 minute: 30 second: 15.
	self assert: (self materialize: 'Time[''06:30:15'']') equals: time.
	time := Time hour: 6 minute: 30 second: 15 nanoSecond: 123.
	self assert: (self materialize: 'Time[''06:30:15.000000123'']') equals: time.
