tests
testMinutes
	self assert: aDuration   minutes = 3.
	self assert: (Duration minutes: 3) minutes = 3.	