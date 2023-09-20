"
SUnit tests for fuel serializations, here signatures
"
Class {
	#name : 'FLSignatureTest',
	#superclass : 'FLSerializationTest',
	#category : 'Fuel-Core-Tests-Base',
	#package : 'Fuel-Core-Tests',
	#tag : 'Base'
}

{ #category : 'tests' }
FLSignatureTest >> testBadSignature [
	self serializer
		at: #signature
		put: 'FUELx'.
	self materializer
		at: #signature
		put: 'FUELy'.
		
	self
		should: [ self resultOfSerializeAndMaterialize: 'content' ]
		raise: FLBadSignature
		whoseDescriptionIncludes: 'FUELy'
		description: 'The materializer should raise an error when signature differs of the expected one.'
]

{ #category : 'tests' }
FLSignatureTest >> testSameSignature [
	self serializer
		at: #signature
		put: 'FUELx'.
	self materializer
		at: #signature
		put: 'FUELx'.
	self assertSerializationEqualityOf: 'content'
]
