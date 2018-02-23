"
A trait for test purposes
"
Trait {
	#name : #TIncludesForIdentityCollectionsTest,
	#category : #'Collections-Tests-Abstract'
}

{ #category : #requirements }
TIncludesForIdentityCollectionsTest >> identityCollectionWithElementsCopyNotIdentical [
	" return a collection including elements for which #copy return a new object "
	^ self explicitRequirement
]

{ #category : #'tests - fixture' }
TIncludesForIdentityCollectionsTest >> test0FixtureInludesForIdentityCollectionsTest [
	self identityCollectionWithElementsCopyNotIdentical.
	self identityCollectionWithElementsCopyNotIdentical do: [ :each | self deny: each == each copy ]
]

{ #category : #'tests - including for identity collections' }
TIncludesForIdentityCollectionsTest >> testIdentityIncludesForIdentityCollections [
	" test the comportement in presence of elements 'includes' but not 'identityIncludes' "
	" can not be used by collections that can't include elements for wich copy doesn't return another instance "
	| collection element |
	collection := self identityCollectionWithElementsCopyNotIdentical .
	element := collection anyOne copy.
	self deny: (collection identityIncludes: element)
]

{ #category : #'tests - including for identity collections' }
TIncludesForIdentityCollectionsTest >> testIncludesAllOfForIdentityCollections [

	| collection copyCollection |
	collection := self identityCollectionWithElementsCopyNotIdentical .
	copyCollection := OrderedCollection new.
	collection do: [ :each | copyCollection add: each copy ].
	self assert: (collection includesAll: collection).
	self deny: (collection includesAll: copyCollection).
	self deny: (collection includesAll: {  (copyCollection anyOne)  })
]

{ #category : #'tests - including for identity collections' }
TIncludesForIdentityCollectionsTest >> testIncludesAnyOfForIdentityCollections [

	| collection copyCollection |
	collection := self identityCollectionWithElementsCopyNotIdentical .
	copyCollection := OrderedCollection new.
	collection do: [ :each | copyCollection add: each copy ].
	self deny: (collection includesAny: copyCollection).
	self assert: (collection includesAny: {  (collection anyOne)  })
]

{ #category : #'tests - including for identity collections' }
TIncludesForIdentityCollectionsTest >> testIncludesForIdentityCollections [

	| collection element elementCopy |
	collection := self identityCollectionWithElementsCopyNotIdentical .
	element := collection anyOne.
	elementCopy := element copy.
	self assert: (collection includes: element).
	self deny: (collection includes: elementCopy)
]
