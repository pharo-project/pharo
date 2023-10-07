"
SUnit tests for fuel versioning
"
Class {
	#name : 'FLVersionTest',
	#superclass : 'FLSerializationTest',
	#category : 'Fuel-Core-Tests-Base',
	#package : 'Fuel-Core-Tests',
	#tag : 'Base'
}

{ #category : 'tests-serialization' }
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

{ #category : 'tests-serialization' }
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

{ #category : 'tests-serialization' }
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

{ #category : 'tests' }
FLVersionTest >> testEquals [
	| version1 version2 |
	version1 := FLVersion
		newWithMajor: 5
		minor: 0
		patch: 6.
	version2 := FLVersion
		newWithMajor: 5
		minor: 0
		patch: 6.
		
	self assert: version1 equals: version1.
	self assert: version1 equals: version2
]

{ #category : 'tests' }
FLVersionTest >> testLessThanOrEqual [
	| version1 version2 |
	version1 := FLVersion
		newWithMajor: 3
		minor: 4
		patch: 5.
	version2 := FLVersion
		newWithMajor: 3
		minor: 4
		patch: 5.
		
	self assert: version1 <= version1.
	self assert: version1 <= version2.
	
	version2 := FLVersion
		newWithMajor: 4
		minor: 4
		patch: 5.
	self assert: version1 <= version2.
	
	version2 := FLVersion
		newWithMajor: 2
		minor: 4
		patch: 5.
	self deny: version1 <= version2.
	
	version2 := FLVersion
		newWithMajor: 3
		minor: 5
		patch: 5.
	self assert: version1 <= version2.
	
	version2 := FLVersion
		newWithMajor: 3
		minor: 3
		patch: 5.
	self deny: version1 <= version2.
	
	version2 := FLVersion
		newWithMajor: 3
		minor: 4
		patch: 6.
	self assert: version1 <= version2.
	
	version2 := FLVersion
		newWithMajor: 3
		minor: 4
		patch: 4.
	self deny: version1 <= version2
]

{ #category : 'tests' }
FLVersionTest >> testNotEqual [
	| version1 version2 |
	version1 := FLVersion
		newWithMajor: 3
		minor: 4
		patch: 5.
	version2 := FLVersion
		newWithMajor: 4
		minor: 4
		patch: 5.
		
	self deny: version1 equals: version2.
	
	version2 := FLVersion
		newWithMajor: 2
		minor: 4
		patch: 5.
	self deny: version1 equals: version2.
	
	version2 := FLVersion
		newWithMajor: 3
		minor: 5
		patch: 5.
	self deny: version1 equals: version2.
	
	version2 := FLVersion
		newWithMajor: 3
		minor: 3
		patch: 5.
	self deny: version1 equals: version2.
	
	version2 := FLVersion
		newWithMajor: 3
		minor: 4
		patch: 6.
	self deny: version1 equals: version2.
	
	version2 := FLVersion
		newWithMajor: 3
		minor: 4
		patch: 4.
	self deny: version1 equals: version2
]

{ #category : 'tests-serialization' }
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
