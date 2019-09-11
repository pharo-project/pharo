"
A trait for test purposes
"
Trait {
	#name : #TConvertTest,
	#category : #'Collections-Tests-Abstract'
}

{ #category : #asserting }
TConvertTest >> assertNoDuplicates: aCollection whenConvertedTo: aClass [

	| result |
	result := self collectionWithEqualElements asIdentitySet.
	self assert: (result class includesBehavior: IdentitySet).
	self collectionWithEqualElements do: [ :initial | self assert: (result occurrencesOf: initial) equals: 1 ]
]

{ #category : #asserting }
TConvertTest >> assertNonDuplicatedContents: aCollection whenConvertedTo: aClass [

	| result |
	result := aCollection perform: ('as' , aClass name) asSymbol.
	self assert: (result class includesBehavior: aClass).
	result do: 
		[ :each | 
		self assert: (aCollection occurrencesOf: each) equals: (result occurrencesOf: each) ].
	^ result
]

{ #category : #asserting }
TConvertTest >> assertSameContents: aCollection whenConvertedTo: aClass [

	| result |
	result := self assertNonDuplicatedContents: aCollection whenConvertedTo: aClass.
	self assert: result size equals: aCollection size
]

{ #category : #requirements }
TConvertTest >> collectionWithoutEqualElements [
" return a collection without equal elements"
	^ self explicitRequirement
]

{ #category : #requirements }
TConvertTest >> integerCollectionWithoutEqualElements [
" return a collection of integer without equal elements"
	^ self explicitRequirement
]

{ #category : #'tests - fixture' }
TConvertTest >> test0FixtureTConvertTest [
	"A collection of number without equal elements:"

	| res |
	self collectionWithoutEqualElements.
	res := true.
	self collectionWithoutEqualElements
		detect: [ :each | (self collectionWithoutEqualElements occurrencesOf: each) > 1 ]
		ifNone: [ res := false ].
	self assert: res = false
]

{ #category : #'tests - converting' }
TConvertTest >> testAsArray [

	self 
		assertSameContents: self collectionWithoutEqualElements
		whenConvertedTo: Array
]

{ #category : #'tests - converting' }
TConvertTest >> testAsBag [

	self assertSameContents: self collectionWithoutEqualElements whenConvertedTo: Bag
]

{ #category : #'tests - converting' }
TConvertTest >> testAsByteArray [

	| res |
	self integerCollectionWithoutEqualElements.
	self integerCollectionWithoutEqualElements do: [ :each | self assert: each class = SmallInteger ].
	res := true.
	self integerCollectionWithoutEqualElements
		detect: [ :each | (self integerCollectionWithoutEqualElements occurrencesOf: each) > 1 ]
		ifNone: [ res := false ].
	self assert: res = false.
	self assertSameContents: self integerCollectionWithoutEqualElements whenConvertedTo: ByteArray
]

{ #category : #'tests - converting' }
TConvertTest >> testAsIdentitySet [
	"Test with a collection without equal elements :"

	self 
		assertSameContents: self collectionWithoutEqualElements
		whenConvertedTo: IdentitySet

]

{ #category : #'tests - converting' }
TConvertTest >> testAsOrderedCollection [
	
	self assertSameContents: self collectionWithoutEqualElements whenConvertedTo: OrderedCollection
]

{ #category : #'tests - converting' }
TConvertTest >> testAsSet [
 	"Test with a collection without equal elements"
	
	self assertSameContents: self collectionWithoutEqualElements whenConvertedTo: Set
	
]
