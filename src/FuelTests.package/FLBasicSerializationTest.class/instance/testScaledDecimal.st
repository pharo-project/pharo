tests-numbers
testScaledDecimal

	self assertSerializationEqualityOf: (13/11s6).
	self assertSerializationEqualityOf: (-13/11s6).
	self assertSerializationEqualityOf: (7621476292473147/9007199254740992s8).
	self assertSerializationEqualityOf: (-7621476292473147/9007199254740992s8).
	self assertSerializationEqualityOf: (-0/9007199254740992s8).
	self assertSerializationEqualityOf: ((13 / 11) asScaledDecimal: 6).
	self assertSerializationEqualityOf: ((11 / 13) asFloat asScaledDecimal).
