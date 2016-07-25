tests-collections
testArray
	self assertSerializationEqualityOf: #(1).
	self assertSerializationEqualityOf: #(10 20).
	self assertSerializationEqualityOf: #(1 2 3 #('Hello' 2 3)).
	

