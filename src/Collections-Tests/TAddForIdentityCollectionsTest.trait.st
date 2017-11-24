"
A trait for test purposes
"
Trait {
	#name : #TAddForIdentityCollectionsTest,
	#category : #'Collections-Tests-Abstract'
}

{ #category : #requirements }
TAddForIdentityCollectionsTest >> identityCollectionWithElementsCopyNotIdentical [
	"Returns a collection including elements for which #copy doesn't return the same object."
	^ self explicitRequirement
]

{ #category : #'tests - fixture' }
TAddForIdentityCollectionsTest >> test0FixtureAddForIdentityCollectionsTest [
	
	self identityCollectionWithElementsCopyNotIdentical.
	self identityCollectionWithElementsCopyNotIdentical  do: [ : each | self deny: each == each copy ].
]

{ #category : #'tests - adding for identity collections' }
TAddForIdentityCollectionsTest >> testTAddIfNotPresentWithElementCopy [
" test specific to IdentityCollections for wich #addIfNotPresent should use == check insted of =."
	| added oldSize collection element elementCopy |
	collection := self identityCollectionWithElementsCopyNotIdentical   .
	oldSize := collection  size.
	
	element := collection  anyOne .
	elementCopy := element copy.
	self deny: (collection  includes: elementCopy ).
	
	added := collection  addIfNotPresent: elementCopy  .
	self assert: added == elementCopy . "test for identiy because #add: has not reason to copy its parameter."
	self assert: (collection  size = (oldSize + 1)).

	
]
