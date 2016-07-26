tests
testAsMilliSeconds
	"self debug:#testAsMilliSeconds"
	
	self assert: (Duration nanoSeconds: 1000000)  asMilliSeconds = 1.
	self assert: (Duration seconds: 1)  asMilliSeconds = 1000.	
	self assert: (Duration nanoSeconds: 1000000)  asMilliSeconds = 1.
	self assert: (Duration nanoSeconds: 1000000)  asMilliSeconds = 1.
	self assert: aDuration   asMilliSeconds = 93784000.
	self assert: (Duration milliSeconds: 3775) asSeconds = 3.
	self assert: (Duration milliSeconds: 3775) nanoSeconds = 775000000.
	self assert: (Duration milliSeconds: -3775) asSeconds = -3.
	self assert: (Duration milliSeconds: -3775) nanoSeconds = -775000000