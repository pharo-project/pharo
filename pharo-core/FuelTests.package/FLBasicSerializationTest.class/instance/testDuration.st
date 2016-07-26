tests
testDuration
	
	self assertSerializationEqualityOf: 123 seconds.
	self assertSerializationEqualityOf: -123 seconds.
	self assertSerializationEqualityOf: (
		Duration 
			seconds: 3 
			nanoSeconds: 35).
	
