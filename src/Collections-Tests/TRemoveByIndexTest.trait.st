"
A trait for test purposes
"
Trait {
	#name : #TRemoveByIndexTest,
	#category : #'Collections-Tests-Abstract'
}

{ #category : #requirements }
TRemoveByIndexTest >> collectionWith5Elements [
" return a collection of size 5 including 5 elements"
self explicitRequirement
]

{ #category : #requirements }
TRemoveByIndexTest >> empty [
" return an empty collection"
self explicitRequirement
]

{ #category : #'tests - fixture' }
TRemoveByIndexTest >> test0FixtureRemoveByIndexTest [

	self collectionWith5Elements.
	self assert: self collectionWith5Elements size = 5
]

{ #category : #'tests - removing by index' }
TRemoveByIndexTest >> testRemoveAt [

	| collection element result oldSize |
	collection := self collectionWith5Elements .
	element := collection at: 3.
	oldSize := collection size.

	result := collection removeAt: 3.
	self assert: result equals: element.
	self assert: collection size equals: (oldSize - 1).
]

{ #category : #'tests - removing by index' }
TRemoveByIndexTest >> testRemoveAtNotPresent [

	self should: [self empty removeAt: 2] raise: Error
]

{ #category : #'tests - removing by index' }
TRemoveByIndexTest >> testRemoveFirst [

	| collection element result oldSize |
	collection := self collectionWith5Elements.
	element := collection first.
	oldSize := collection size.

	result := collection removeFirst.
	self assert: result equals: element .
	self assert: collection size equals: (oldSize - 1)
]

{ #category : #'tests - removing by index' }
TRemoveByIndexTest >> testRemoveFirstNElements [

	| collection elements result oldSize |
	collection := self collectionWith5Elements .
	elements := { collection first. collection at:2 }.
	oldSize := collection size.

	result := collection removeFirst: 2.
	self assert: result equals: elements .
	self assert: collection size equals: (oldSize - 2)
]

{ #category : #'tests - removing by index' }
TRemoveByIndexTest >> testRemoveFirstNElementsNotPresent [

	self should: [self empty removeFirst: 2] raise: Error
]

{ #category : #'tests - removing by index' }
TRemoveByIndexTest >> testRemoveFirstNotPresent [

	self should: [self empty removeFirst] raise: Error
]

{ #category : #'tests - removing by index' }
TRemoveByIndexTest >> testRemoveLast [

	| collection element result oldSize |
	collection := self collectionWith5Elements .
	element := collection last.
	oldSize := collection size.

	result := collection removeLast.
	self assert: result equals: element .
	self assert: collection size equals: (oldSize - 1)
]

{ #category : #'tests - removing by index' }
TRemoveByIndexTest >> testRemoveLastNElements [

	| collection result oldSize elements |
	collection := self collectionWith5Elements .
	elements := { collection at: 4. collection last }.
	oldSize := collection size.

	result := (collection removeLast: 2).
	self assert: result equals: elements.
	self assert: collection size equals: (oldSize - 2)
]

{ #category : #'tests - removing by index' }
TRemoveByIndexTest >> testRemoveLastNElementsNElements [

	self should: [self empty removeLast: 2] raise: Error
]

{ #category : #'tests - removing by index' }
TRemoveByIndexTest >> testRemoveLastNotPresent [

	self should: [self empty removeLast] raise: Error
]
