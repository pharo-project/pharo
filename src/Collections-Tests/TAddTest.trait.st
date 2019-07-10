"
A trait for test purposes
"
Trait {
	#name : #TAddTest,
	#category : #'Collections-Tests-Abstract'
}

{ #category : #requirements }
TAddTest >> collectionWithElement [
	"Returns a collection that already includes what is returned by #element."
	^ self explicitRequirement
]

{ #category : #requirements }
TAddTest >> element [
	"Returns an object that can be added to the collection returned by #collection."
	^ self explicitRequirement
]

{ #category : #requirements }
TAddTest >> otherCollection [
	"Returns a collection that does not include what is returned by #element."
	^ self explicitRequirement
]

{ #category : #'tests - fixture' }
TAddTest >> test0FixtureRequirementsOfTAddTest [

	self collectionWithElement.
	self otherCollection.
	self element.
	self assert: (self collectionWithElement includes: self element).
	self deny: (self otherCollection includes: self element)
]

{ #category : #'tests - adding' }
TAddTest >> testTAdd [
	| added collection |
	collection := self otherCollection.
	added := collection add: self element.
	
	self assert: added == self element.	"test for identiy because #add: has not reason to copy its parameter."
	self assert: (collection includes: self element)	.
	self assert: (self collectionWithElement includes: self element).
	
	
]

{ #category : #'tests - adding' }
TAddTest >> testTAddAll [

	| added collection toBeAdded |
	collection := self collectionWithElement.
	toBeAdded := self otherCollection.
	added := collection addAll: toBeAdded.
	self assert: added == toBeAdded .	"test for identiy because #addAll: has not reason to copy its parameter."
	self assert: (collection includesAll: toBeAdded )
]

{ #category : #'tests - adding' }
TAddTest >> testTAddIfNotPresentWithElementAlreadyIn [

	| added oldSize collection anElement |
	collection := self collectionWithElement.
	oldSize := collection size.
	anElement := self element.
	self assert: (collection includes: anElement).
	
	added := collection addIfNotPresent: anElement.
	
	self assert: added == anElement .	"test for identiy because #add: has not reason to copy its parameter."
	self assert: collection size equals: oldSize
]

{ #category : #'tests - adding' }
TAddTest >> testTAddIfNotPresentWithNewElement [

	| added oldSize collection elem |
	collection := self otherCollection.
	oldSize := collection size.
	elem := self element.
	self deny: (collection includes: elem ).
	
	added := collection  addIfNotPresent: elem .
	self assert: added == elem . "test for identiy because #add: has not reason to copy its parameter."
	self assert: collection size equals: (oldSize + 1).

	
]

{ #category : #'tests - adding' }
TAddTest >> testTAddTwice [

	| added oldSize collection anElement |
	collection := self collectionWithElement.
	anElement := self element.
	oldSize := collection size.
	added := collection 
		add: anElement;
		add: anElement.
	self assert: added == anElement.	"test for identiy because #add: has not reason to copy its parameter."
	self assert: (collection includes: anElement).
	self assert: collection size equals: (oldSize + 2)
]

{ #category : #'tests - adding' }
TAddTest >> testTAddWithOccurences [
	| added oldSize collection anElement |
	collection := self collectionWithElement.
	anElement := self element.
	oldSize := collection size.
	added := collection add: anElement withOccurrences: 5.
	
	self assert: added == anElement.	"test for identiy because #add: has not reason to copy its parameter."
	self assert: (collection includes: anElement).
	self assert: collection size equals: (oldSize + 5)
]
