tests
testTotalSeconds
	self assert: 0 seconds totalSeconds equals: 0.
	self assert: 1 second totalSeconds equals: 1.
	self assert: 100 second totalSeconds equals: 100.
	self assert: 500 milliSeconds totalSeconds equals: 1/2.
	self assert: 1500 milliSeconds totalSeconds equals: 3/2.
	self assert: 2500 milliSeconds totalSeconds equals: 5/2.
	self assert: 1 second negated totalSeconds equals: -1