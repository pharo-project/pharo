tests
testAssociation

	self assertSerializationEqualityOf: 1-> 'marino'.
	self assertSerializationEqualityOf: nil-> 'marino'.
	self assertSerializationEqualityOf: nil-> nil.
	self assertSerializationEqualityOf: nil-> #(1 3 4).
	self assertSerializationEqualityOf: nil-> true.