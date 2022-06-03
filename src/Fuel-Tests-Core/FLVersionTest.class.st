"
SUnit tests for fuel versioning
"
Class {
	#name : #FLVersionTest,
	#superclass : #FLSerializationTest,
	#category : #'Fuel-Tests-Core-Base'
}

{ #category : #tests }
FLVersionTest >> testDifferentMajor [
	self serializer
		at: #version
		put: (FLVersion
			newWithMajor: 2
			minor: 2
			patch: 3).
	self materializer
		at: #version
		put: (FLVersion
			newWithMajor: 3
			minor: 2
			patch: 3).
	self
		should: [ self resultOfSerializeAndMaterialize: 'content' ]
		raise: FLBadVersion
		whoseDescriptionIncludes: '2'
		description: 'The materializer should raise an error when version differs of the expected one.'
]

{ #category : #tests }
FLVersionTest >> testDifferentMinor [
	self serializer
		at: #version
		put: (FLVersion
			newWithMajor: 1
			minor: 2
			patch: 3).
	self materializer
		at: #version
		put: (FLVersion
			newWithMajor: 1
			minor: 3
			patch: 3).
	self assertSerializationEqualityOf: 'content'
]

{ #category : #tests }
FLVersionTest >> testDifferentPatch [
	self serializer
		at: #version
		put: (FLVersion
			newWithMajor: 1
			minor: 2
			patch: 3).
	self materializer
		at: #version
		put: (FLVersion
			newWithMajor: 1
			minor: 2
			patch: 4).
	self assertSerializationEqualityOf: 'content'
]

{ #category : #tests }
FLVersionTest >> testSameVersion [
	self serializer
		at: #version
		put: (FLVersion
			newWithMajor: 1
			minor: 2
			patch: 3).
	self materializer
		at: #version
		put: (FLVersion
			newWithMajor: 1
			minor: 2
			patch: 3).
	self assertSerializationEqualityOf: 'content'
]
