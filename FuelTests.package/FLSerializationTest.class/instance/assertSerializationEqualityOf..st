running
assertSerializationEqualityOf: anObject
	"Asserts that the original object and the materialized one are equal (but not the same)"

	| materialized |
	materialized := self resultOfSerializeAndMaterialize: anObject.
	self assert: anObject ~~ materialized description: 'The materialized object should not be the same as the serialized one'.
	self assert: anObject = materialized description: 'The materialized object should be equal to serialized one'.