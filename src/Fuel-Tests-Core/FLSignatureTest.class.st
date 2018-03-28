"
SUnit tests for fuel serializations, here signatures
"
Class {
	#name : #FLSignatureTest,
	#superclass : #FLSerializationTest,
	#category : 'Fuel-Tests-Core-Base'
}

{ #category : #tests }
FLSignatureTest >> testBadSignature [

	serializer signature: 'FUELx'.
	materializer signature: 'FUELy'.
	self
		should: [ self resultOfSerializeAndMaterialize: 'content' ]
		raise: FLBadSignature
		whoseDescriptionIncludes: 'FUELy'
		description: 'The materializer should raise an error when signature differs of the expected one.'
]

{ #category : #tests }
FLSignatureTest >> testSameSignature [

	serializer signature: 'FUELx'.
	materializer signature: 'FUELx'.
	self assertSerializationEqualityOf: 'content'
]
