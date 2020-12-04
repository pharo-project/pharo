"
A trait for test purposes
"
Trait {
	#name : #TConcatenationEqualElementsRemovedTest,
	#traits : 'TConcatenationTest',
	#classTraits : 'TConcatenationTest classTrait',
	#category : #'Collections-Abstract-Tests-Traits'
}

{ #category : #requirements }
TConcatenationEqualElementsRemovedTest >> empty [
	self explicitRequirement.
]

{ #category : #requirements }
TConcatenationEqualElementsRemovedTest >> firstCollection [
" return a collection that will be the first part of the concatenation"
	self explicitRequirement
]

{ #category : #requirements }
TConcatenationEqualElementsRemovedTest >> secondCollection [
" return a collection that will be the second part of the concatenation"
	self explicitRequirement
]

{ #category : #'tests - fixture' }
TConcatenationEqualElementsRemovedTest >> test0FixtureConcatenationTest [
	self firstCollection.
	self denyEmpty: self firstCollection.
	self firstCollection.
	self denyEmpty: self firstCollection.
	self empty.
	self assertEmpty: self empty
]

{ #category : #'tests - concatenation' }
TConcatenationEqualElementsRemovedTest >> testConcatenation [

| collection1 collection2 result |
collection1 := self firstCollection .
collection2 := self secondCollection .
result := collection1 , collection2.

collection1 do:[ :each | self assert: (result includes: each)].
collection2 do:[ :each | self assert: (result includes: each)].

]

{ #category : #'tests - concatenation' }
TConcatenationEqualElementsRemovedTest >> testConcatenationWithDuplicate [


| collection1 collection2 result |
collection1 := self firstCollection .
collection2 := self firstCollection  .
result := collection1 , collection2.

collection1 do:[ :each | self assert: (result includes: each)].
self assert: result size = collection1 size.
]

{ #category : #'tests - concatenation' }
TConcatenationEqualElementsRemovedTest >> testConcatenationWithEmpty [
	| result |
	result := self firstCollection , self empty.
	self assert: result = self firstCollection
]
