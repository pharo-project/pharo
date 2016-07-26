tests-numbers
testFloat
	
	self assertSerializationEqualityOf: 180.0.
	self assertSerializationEqualityOf: 0.0.
	self assertSerializationEqualityOf: -0.0.
	self assertSerializationEqualityOf: 11.22321.
	self assertSerializationEqualityOf: -11.22321.
	self assertSerializationEqualityOf: -132311.22321.
	self assertSerializationEqualityOf: 1234567890.123456789.
	self assertSerializationEqualityOf: -1234567890.123456789.
	self assertSerializationEqualityOf: Float e.
	self assertSerializationEqualityOf: Float infinity.
	self assertSerializationEqualityOf: Float halfPi.
	self assertSerializationEqualityOf: Float negativeZero.
	self assertSerializationEqualityOf: Float halfPi.
	self assertSerializationEqualityOf: Float pi.
