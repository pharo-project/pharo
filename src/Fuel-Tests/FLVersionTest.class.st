"
SUnit tests for fuel versioning
"
Class {
	#name : #FLVersionTest,
	#superclass : #FLSerializationTest,
	#category : #'Fuel-Tests-Core'
}

{ #category : #tests }
FLVersionTest >> testBadVersion [

	serializer version: 2.
	materializer version: 1.
	self
		should: [ self resultOfSerializeAndMaterialize: 'content' ]
		raise: FLBadVersion
		whoseDescriptionIncludes: '2'
		description: 'The materializer should raise an error when version differs of the expected one.'
]

{ #category : #tests }
FLVersionTest >> testSameVersion [

	serializer version: 1.
	materializer version: 1.
	self assertSerializationEqualityOf: 'content'
]
