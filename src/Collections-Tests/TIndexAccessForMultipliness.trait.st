"
A trait for test purposes
"
Trait {
	#name : #TIndexAccessForMultipliness,
	#category : #'Collections-Tests-Abstract'
}

{ #category : #requirements }
TIndexAccessForMultipliness >> collectionWithNonIdentitySameAtEndAndBegining [
	" return a collection with elements at end and begining equals only with classic equality (they are not the same object).
(others elements of the collection are not equal to those elements)"
	self explicitRequirement
]

{ #category : #requirements }
TIndexAccessForMultipliness >> collectionWithSameAtEndAndBegining [
	" return a collection with elements at end and begining equals .
(others elements of the collection are not equal to those elements)"
	self explicitRequirement
]

{ #category : #'tests - fixture' }
TIndexAccessForMultipliness >> test0FixtureIndexAccessFotMultipliness [
	self collectionWithSameAtEndAndBegining.
	self assert: self collectionWithSameAtEndAndBegining first = self collectionWithSameAtEndAndBegining last.
	self assert: self collectionWithSameAtEndAndBegining size > 1.
	1 to: self collectionWithSameAtEndAndBegining size do: [ :i | 
		i > 1 & (i < self collectionWithSameAtEndAndBegining size)
			ifTrue: [ self deny: (self collectionWithSameAtEndAndBegining at: i) = self collectionWithSameAtEndAndBegining first ] ]
]

{ #category : #'tests - index accessing for multipliness' }
TIndexAccessForMultipliness >> testIdentityIndexOfDuplicate [

	| collection element |
	"testing fixture here as this method may not be used by some collections testClass"
	self collectionWithNonIdentitySameAtEndAndBegining.
	collection := self collectionWithNonIdentitySameAtEndAndBegining.
	self assert: collection first = collection last.
	self deny: collection first == collection last.
	1 to: collection size do: [ :i | 
		i > 1 & (i < collection size)
			ifTrue: [ self deny: (collection at: i) = collection first ] ].
	element := collection last.	" floatCollectionWithSameAtEndAndBegining first and last elements are equals but are not the same object"
	self assert: (collection identityIndexOf: element) = collection size
]

{ #category : #'tests - index accessing for multipliness' }
TIndexAccessForMultipliness >> testIdentityIndexOfIAbsentDuplicate [

	| collection element elementCopy |
	collection := self collectionWithNonIdentitySameAtEndAndBegining .
	element := collection last.
	elementCopy := element copy.
	self deny: element  == elementCopy .
	self assert: (collection 
			identityIndexOf: element
			ifAbsent: [ 0 ]) = collection size.
	self assert: (collection 
			identityIndexOf: elementCopy
			ifAbsent: [ 55 ]) = 55
]

{ #category : #'tests - index accessing for multipliness' }
TIndexAccessForMultipliness >> testIndexOfDuplicate [

	| collection element |
	collection := self collectionWithSameAtEndAndBegining.
	element := collection last.

	" floatCollectionWithSameAtEndAndBegining first and last elements are equals 
	'indexOf: should return the position of the first occurrence :'"
	self assert: (collection indexOf: element) = 1
]

{ #category : #'tests - index accessing for multipliness' }
TIndexAccessForMultipliness >> testIndexOfIfAbsentDuplicate [

	| collection element |
	collection := self collectionWithSameAtEndAndBegining.
	element := collection last.

	" floatCollectionWithSameAtEndAndBegining first and last elements are equals 
	'indexOf:ifAbsent: should return the position of the first occurrence :'"
	self assert: (collection 
			indexOf: element
			ifAbsent: [ 55 ]) = 1
]

{ #category : #'tests - index accessing for multipliness' }
TIndexAccessForMultipliness >> testIndexOfStartingAtDuplicate [

	| collection element |
	collection := self collectionWithSameAtEndAndBegining.
	element := collection last.

	" floatCollectionWithSameAtEndAndBegining first and last elements are equals 
	'indexOf:ifAbsent:startingAt: should return the position of the first occurrence :'"
	self assert: (collection 
			indexOf: element
			startingAt: 1
			ifAbsent: [ 55 ]) = 1.
	self assert: (collection 
			indexOf: element
			startingAt: 2
			ifAbsent: [ 55 ]) = collection size
]

{ #category : #'tests - index accessing for multipliness' }
TIndexAccessForMultipliness >> testLastIndexOfDuplicate [

	| collection element |
	collection := self collectionWithSameAtEndAndBegining.
	element := collection first.

	" floatCollectionWithSameAtEndAndBegining first and last elements are equals 
	'lastIndexOf: should return the position of the last occurrence :'"
	self assert: (collection lastIndexOf: element) = collection size
]

{ #category : #'tests - index accessing for multipliness' }
TIndexAccessForMultipliness >> testLastIndexOfIfAbsentDuplicate [
	| collection element |
	collection := self collectionWithSameAtEndAndBegining.
	element := collection first.

	" floatCollectionWithSameAtEndAndBegining first and last elements are equals 
	'lastIndexOf: should return the position of the last occurrence :'"
	self assert: (collection 
			lastIndexOf: element
			ifAbsent: [ 55 ]) = collection size
]

{ #category : #'tests - index accessing for multipliness' }
TIndexAccessForMultipliness >> testLastIndexOfStartingAtDuplicate [
	| collection element |
	collection := self collectionWithSameAtEndAndBegining.
	element := collection last.

	" floatCollectionWithSameAtEndAndBegining first and last elements are equals 
	'lastIndexOf:ifAbsent:startingAt: should return the position of the last occurrence :'"
	self assert: (collection 
			lastIndexOf: element
			startingAt: collection size
			ifAbsent: [ 55 ]) = collection size.
	self assert: (collection 
			lastIndexOf: element
			startingAt: collection size - 1
			ifAbsent: [ 55 ]) = 1
]
