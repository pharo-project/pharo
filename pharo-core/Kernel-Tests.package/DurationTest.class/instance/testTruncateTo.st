tests
testTruncateTo

	self assert: ((5 minutes + 37 seconds) truncateTo: (2 minutes)) = (4 minutes).
	self assert:  (aDuration truncateTo: (Duration days: 1)) =
	               (Duration days: 1 hours: 0 minutes: 0 seconds: 0 nanoSeconds: 0).
	self assert:  (aDuration truncateTo: (Duration hours: 1)) =
	               (Duration days: 1 hours: 2 minutes: 0 seconds: 0 nanoSeconds: 0).	
	self assert:  (aDuration truncateTo: (Duration minutes: 1)) =
	               (Duration days: 1 hours: 2 minutes: 3 seconds: 0 nanoSeconds: 0).