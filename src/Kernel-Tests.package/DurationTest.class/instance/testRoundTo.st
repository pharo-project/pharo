tests
testRoundTo

	self assert: ((5 minutes + 37 seconds) roundTo: (2 minutes)) = (6 minutes).
	
	self assert:  (aDuration roundTo: (Duration days: 1)) =
	               (Duration days: 1 hours: 0 minutes: 0 seconds: 0 nanoSeconds: 0).
	self assert:  (aDuration roundTo: (Duration hours: 1)) =
	               (Duration days: 1 hours: 2 minutes: 0 seconds: 0 nanoSeconds: 0).	
	self assert:  (aDuration roundTo: (Duration minutes: 1)) =
	               (Duration days: 1 hours: 2 minutes: 3 seconds: 0 nanoSeconds: 0).