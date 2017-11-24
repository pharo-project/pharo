"
A trait for test purposes
"
Trait {
	#name : #TConcatenationTest,
	#category : #'Collections-Tests-Abstract'
}

{ #category : #requirements }
TConcatenationTest >> empty [
	self explicitRequirement.
]

{ #category : #requirements }
TConcatenationTest >> firstCollection [
" return a collection that will be the first part of the concatenation"
	self explicitRequirement
]

{ #category : #requirements }
TConcatenationTest >> secondCollection [
" return a collection that will be the second part of the concatenation"
	self explicitRequirement
]

{ #category : #'tests - fixture' }
TConcatenationTest >> test0FixtureConcatenationTest [
	self firstCollection.
	self deny: self firstCollection isEmpty.
	self firstCollection.
	self deny: self firstCollection isEmpty.
	self empty.
	self assert: self empty isEmpty
]

{ #category : #'tests - concatenation' }
TConcatenationTest >> testConcatenation [

"| collection1 collection2 result |
collection1 := self firstCollection .
collection2 := self secondCollection .
result := collection1 , collection2.

collection1 do:[ :each | self assert: (result includes: each)].
collection2 do:[ :each | self assert: (result includes: each)]."

| collection1 collection2 result |
collection1 := self firstCollection .
collection2 := self secondCollection .
result := collection1 , collection2.

result do: [ :each | self assert: (result occurrencesOf: each) = (( collection1 occurrencesOf: each ) + ( collection2 occurrencesOf: each ) ). ].
self assert: result size = (collection1 size + collection2 size)
]

{ #category : #'tests - concatenation' }
TConcatenationTest >> testConcatenationWithDuplicate [


| collection1 collection2 result |
collection1 := self firstCollection .
collection2 := self firstCollection .
result := collection1 , collection2.

result do: [ :each | self assert: (result occurrencesOf: each) = (( collection1 occurrencesOf: each ) + ( collection2 occurrencesOf: each ) ). ].
self assert: result size = (collection1 size * 2)
]

{ #category : #'tests - concatenation' }
TConcatenationTest >> testConcatenationWithEmpty [
	| result |
	result := self firstCollection , self empty.
	self assert: result = self firstCollection
]
