tests
testMinus
	self assert: aDuration - aDuration = (Duration seconds: 0).
	self assert: aDuration - (Duration days: -1 hours: -2 minutes: -3 seconds: -4 nanoSeconds: -5) = 
						    (Duration days: 2  hours: 4  minutes: 6  seconds: 8  nanoSeconds: 10). 
	self assert: aDuration - (Duration days: 0  hours: 1  minutes: 2  seconds: 3  nanoSeconds: 4) = 
						    (Duration days: 1  hours: 1  minutes: 1  seconds: 1  nanoSeconds: 1). 
	self assert: aDuration - (Duration days: 0  hours: 3   minutes: 0  seconds: 5  nanoSeconds: 0) = 
						    (Duration days: 0  hours: 23  minutes: 2  seconds: 59  nanoSeconds: 5). 