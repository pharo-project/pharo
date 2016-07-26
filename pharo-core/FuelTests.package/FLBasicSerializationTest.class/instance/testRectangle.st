tests
testRectangle
	
	self assertSerializationEqualityOf: (10@20 corner: 30@40).
	self assertSerializationEqualityOf: (1.1@2.2 corner: 3.3@4.4).
	self assertSerializationEqualityOf: (1.1@ -2.2 corner: -3.3@4.4).