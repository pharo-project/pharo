"
A trait for test purposes
"
Trait {
	#name : #TOccurrencesForMultiplinessTest,
	#traits : 'TOccurrencesTest',
	#classTraits : 'TOccurrencesTest classTrait',
	#category : #'Collections-Abstract-Tests-Traits'
}

{ #category : #requirements }
TOccurrencesForMultiplinessTest >> collectionWithEqualElements [
" return a collecition including atLeast two elements equal"

^ self explicitRequirement.
]

{ #category : #requirements }
TOccurrencesForMultiplinessTest >> collectionWithoutEqualElements [
	self explicitRequirement
]

{ #category : #requirements }
TOccurrencesForMultiplinessTest >> elementNotInForOccurrences [
" return an element notIncluded in #collectionWithoutEqualElements"
	self explicitRequirement
]

{ #category : #requirements }
TOccurrencesForMultiplinessTest >> elementTwiceInForOccurrences [
" return an element included exactly two time in # collectionWithEqualElements"
^ self explicitRequirement
]

{ #category : #requirements }
TOccurrencesForMultiplinessTest >> empty [
	self explicitRequirement
]

{ #category : #'tests - fixture' }
TOccurrencesForMultiplinessTest >> test0FixtureOccurrencesForMultiplinessTest [
	| cpt |
	self collectionWithEqualElements.
	self collectionWithEqualElements.
	self elementTwiceInForOccurrences.
	self elementTwiceInForOccurrences.
	self collectionWithEqualElements.
	cpt := 0. " testing with identity check ( == ) so that identy collections can use this trait : "
	self collectionWithEqualElements do: [ :each | 
		each == self elementTwiceInForOccurrences ifTrue: [ cpt := cpt + 1 ] ].
	self assert: cpt = 2
]

{ #category : #'tests - fixture' }
TOccurrencesForMultiplinessTest >> test0FixtureOccurrencesTest [
	| tmp |
	self empty.
	self assertEmpty: self empty.
	self collectionWithoutEqualElements.
	self denyEmpty: self collectionWithoutEqualElements.
	tmp := OrderedCollection new.
	self collectionWithoutEqualElements
		do: [ :each | 
			self deny: (tmp includes: each).
			tmp add: each ].
	self elementNotInForOccurrences.
	self deny: (self collectionWithoutEqualElements includes: self elementNotInForOccurrences)
]

{ #category : #'tests - occurrencesOf' }
TOccurrencesForMultiplinessTest >> testOccurrencesOf [
	| collection |
	collection := self collectionWithoutEqualElements .
	
	collection do: [ :each | self assert: (collection occurrencesOf: each) equals: 1 ]
]

{ #category : #'tests - occurrencesOf' }
TOccurrencesForMultiplinessTest >> testOccurrencesOfEmpty [

	| result |
	result := self empty occurrencesOf: (self collectionWithoutEqualElements anyOne).
	self assert: result equals: 0
]

{ #category : #'tests - occurrencesOf for multipliness' }
TOccurrencesForMultiplinessTest >> testOccurrencesOfForMultipliness [

	| collection elem |
	collection := self collectionWithEqualElements.
	elem := self elementTwiceInForOccurrences.

	self assert: (collection occurrencesOf: elem ) equals: 2.  
]

{ #category : #'tests - occurrencesOf' }
TOccurrencesForMultiplinessTest >> testOccurrencesOfNotIn [

	| result |
	result := self collectionWithoutEqualElements occurrencesOf: self elementNotInForOccurrences.
	self assert: result equals: 0
]
