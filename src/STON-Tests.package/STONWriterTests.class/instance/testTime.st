tests
testTime
	| time |
	time := Time hour: 6 minute: 30 second: 15.
	self assert: (self serialize: time) = 'Time[''06:30:15'']'.
