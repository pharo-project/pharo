tests
testFraction

	self assertSerializationEqualityOf: (3 / 4).
	self assertSerializationEqualityOf: (4 / 3).
	self assertSerializationEqualityOf: (-4 / 3).
	self assertSerializationEqualityOf: (-4 / -3).
	self assertSerializationEqualityOf: (4 / -3).	
	self assertSerializationIdentityOf: (0-0 / -3).	
	self assertSerializationEqualityOf: 0-(1001/1000).	


