running
assertSerializationIdentityOf: anObject
	"Asserts that the original object and the materialized one are the same"

	| result |
	result := self resultOfSerializeAndMaterialize: anObject.
	self 
		assert: anObject == result 
		description: 'The materialized object is not the same as the serialized one'.