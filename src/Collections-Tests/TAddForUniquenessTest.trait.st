"
A trait for test purposes
"
Trait {
	#name : #TAddForUniquenessTest,
	#category : #'Collections-Tests-Abstract'
}

{ #category : #requirements }
TAddForUniquenessTest >> collectionWithElement [
" return a collection already including 'element'    "
	^ self explicitRequirement
]

{ #category : #requirements }
TAddForUniquenessTest >> collectionWithoutElement [
	" return a collection that does not include 'element' "
	^ self explicitRequirement
]

{ #category : #requirements }
TAddForUniquenessTest >> element [
	^ self explicitRequirement
]

{ #category : #'tests - fixture' }
TAddForUniquenessTest >> test0FixtureAddForUniquenessTest [
	self element.
	self collectionWithElement.
	self assert: (self collectionWithElement includes: self element).
	self collectionWithoutElement.
	self deny: (self collectionWithoutElement includes: self element)
]

{ #category : #'tests - adding uniquely' }
TAddForUniquenessTest >> testAddAlreadyThereDoesNotCount [

	| added oldSize |
	oldSize := self collectionWithElement size.
	self assert: (self collectionWithElement includes: self element).

	added := self collectionWithElement add: self element.
	
	self assert: added = self element.
	self assert: (self collectionWithElement includes: self element).
	self assert: self collectionWithElement size = oldSize.
]

{ #category : #'tests - adding uniquely' }
TAddForUniquenessTest >> testAddNewElementIncrementsSize [
	| added oldSize |
	oldSize := self collectionWithoutElement size.
	self deny: (self collectionWithoutElement includes: self element).
	added := self collectionWithoutElement add: self element.
	self assert: added = self element.
	self assert: (self collectionWithoutElement includes: self element).
	self assert: self collectionWithoutElement size = (oldSize + 1)
]

{ #category : #'tests - adding uniquely' }
TAddForUniquenessTest >> testTAddIfNotPresentWithElementAlreadyIn [

	| added oldSize |
	oldSize := self collectionWithElement size.
	self assert: (self collectionWithElement includes: self element).

	added := self collectionWithElement addIfNotPresent: self element.
	
	self assert: added = self element.
	self assert: (self collectionWithElement includes: self element).
	self assert: self collectionWithElement size = oldSize.
]

{ #category : #'tests - adding uniquely' }
TAddForUniquenessTest >> testTAddIfNotPresentWithNewElement [
	| added oldSize |
	oldSize := self collectionWithoutElement size.
	self deny: (self collectionWithoutElement includes: self element).
	added := self collectionWithoutElement addIfNotPresent: self element.
	self assert: added = self element.
	self assert: (self collectionWithoutElement includes: self element).
	self assert: self collectionWithoutElement size = (oldSize + 1)
]
