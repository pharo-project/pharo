tests
testSeconds
	
	self assert: (Time readFrom: '20:33:14.321-05:00' readStream) asDuration seconds = 14