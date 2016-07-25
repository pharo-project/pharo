tests-collections
testInterval

	self assertSerializationEqualityOf: (-10 to: 10 by: 5).
	self assertSerializationEqualityOf: (-0 to: 0 by: 1).
	self assertSerializationEqualityOf: (1 to: 10 by: 2).
	self assertSerializationEqualityOf: (33333333333333331 to: 1444444444444444440 by: 2).
	self assertSerializationEqualityOf: (0 to: 1 by: 2).
	