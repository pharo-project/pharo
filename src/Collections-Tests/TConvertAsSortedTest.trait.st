"
A trait for test purposes
"
Trait {
	#name : #TConvertAsSortedTest,
	#category : 'Collections-Tests-Abstract'
}

{ #category : #requirements }
TConvertAsSortedTest >> collectionWithSortableElements [
" return a collection elements that can be sorte ( understanding message ' < '  or ' > ')"
	^ self explicitRequirement
]

{ #category : #'tests - fixture' }
TConvertAsSortedTest >> test0FixtureConverAsSortedTest [
	self collectionWithSortableElements.
	self deny: self collectionWithSortableElements isEmpty
]

{ #category : #'tests - as sorted collection' }
TConvertAsSortedTest >> testAsSortedArray [
	| result collection |
	collection := self collectionWithSortableElements .
	result := collection  asArray sort.
	self assert: (result class includesBehavior: Array).
	self assert: result isSorted.
	self assert: result size = collection size
]

{ #category : #'tests - as sorted collection' }
TConvertAsSortedTest >> testAsSortedCollection [

	| aCollection result |
	aCollection := self collectionWithSortableElements .
	result := aCollection asSortedCollection.
	
	self assert: (result class includesBehavior: SortedCollection).
	result do: 
		[ :each | 
		self assert: (aCollection occurrencesOf: each) = (result occurrencesOf: each) ].
	
	self assert: result size = aCollection size.
]

{ #category : #'tests - as sorted collection' }
TConvertAsSortedTest >> testAsSortedCollectionWithSortBlock [
	| result tmp |
	result := self collectionWithSortableElements  asSortedCollection: [:a :b | a > b].
	self assert: (result class includesBehavior: SortedCollection).
	result do: 
		[ :each | 
		self assert: (self collectionWithSortableElements   occurrencesOf: each) = (result occurrencesOf: each) ].
	self assert: result size = self collectionWithSortableElements  size.
	tmp:=result at: 1.
	result do: [:each| self assert: tmp>=each. tmp:=each].
	
]
