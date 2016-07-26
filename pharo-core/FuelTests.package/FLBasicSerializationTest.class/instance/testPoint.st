tests
testPoint

	self assertSerializationEqualityOf: 10@20.
	self assertSerializationEqualityOf: -10@20.
	self assertSerializationEqualityOf: -10@ -20.
	self assertSerializationEqualityOf: 1.1@2.2.
	self assertSerializationEqualityOf: 1.1@ -2.2.
	