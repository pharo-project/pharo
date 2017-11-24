"
A trait for test purposes
"
Trait {
	#name : #TOccurrencesForIdentityCollectionsTest,
	#category : #'Collections-Tests-Abstract'
}

{ #category : #requirements }
TOccurrencesForIdentityCollectionsTest >> identityCollectionWithElementsCopyNotIdentical [
" return a collection including elements for which #copy return a new object "
^ self explicitRequirement
]

{ #category : #'tests - fixture' }
TOccurrencesForIdentityCollectionsTest >> test0FixtureOccurrencesForIdentityCollectionsTest [
	self identityCollectionWithElementsCopyNotIdentical.
	self identityCollectionWithElementsCopyNotIdentical do: [ :each | self deny: each copy == each ]
]

{ #category : #'tests - occurrencesOf for identity collections' }
TOccurrencesForIdentityCollectionsTest >> testOccurencesOfForIdentityCollections [

| collection element elementCopy |
collection := self identityCollectionWithElementsCopyNotIdentical .
element := collection anyOne.
elementCopy := element copy.

self assert: (collection occurrencesOf: elementCopy ) = 0.
]
