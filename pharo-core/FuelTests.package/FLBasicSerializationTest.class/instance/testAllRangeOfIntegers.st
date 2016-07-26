tests-numbers
testAllRangeOfIntegers

		self assertSerializationIdentityOf: 100.
		self assertSerializationIdentityOf: 10000.
		self assertSerializationIdentityOf: 100000.
		self assertSerializationIdentityOf: 10000000.
		self assertSerializationIdentityOf: 100000000.
		self assertSerializationIdentityOf: 1000000000.
		self assertSerializationEqualityOf: 3000000000.
		self assertSerializationEqualityOf: 10000000000.
		self assertSerializationEqualityOf: 100000000000.
		self assertSerializationEqualityOf: 100 factorial.

		self assertSerializationIdentityOf: -100.
		self assertSerializationIdentityOf: -10000.
		self assertSerializationIdentityOf: -100000.
		self assertSerializationIdentityOf: -10000000.
		self assertSerializationIdentityOf: -100000000.
		self assertSerializationIdentityOf: -1000000000.
		self assertSerializationEqualityOf: -3000000000.
		self assertSerializationEqualityOf: -10000000000.
		self assertSerializationEqualityOf: -100000000000.
		self assertSerializationEqualityOf:  100 factorial * -1.

