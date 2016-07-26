tests
testAsSeconds
	self assert: (Duration nanoSeconds: 1000000000)  asSeconds = 1.
	self assert: (Duration seconds: 1)  asSeconds = 1.	
	self assert: aDuration asSeconds = 93784.
	self assert: 1 asSeconds equals: (Duration seconds: 1).
	self assert: (1/2) asSeconds equals: (Duration milliSeconds: 500).