"
A trait for test purposes
"
Trait {
	#name : #TSequencedConcatenationTest,
	#category : #'Collections-Tests-Abstract'
}

{ #category : #requirements }
TSequencedConcatenationTest >> empty [
	self explicitRequirement
]

{ #category : #requirements }
TSequencedConcatenationTest >> firstCollection [
" return a collection that will be the first part of the concatenation"
	self explicitRequirement
]

{ #category : #requirements }
TSequencedConcatenationTest >> secondCollection [
" return a collection that will be the second part of the concatenation"
	self explicitRequirement
]

{ #category : #'tests - fixture' }
TSequencedConcatenationTest >> test0FixtureSequencedConcatenationTest [
	self empty.
	self assert: self empty isEmpty.
	self firstCollection.
	self secondCollection
]

{ #category : #'tests - concatenation' }
TSequencedConcatenationTest >> testConcatenation [
	| result index |
	result:= self firstCollection,self secondCollection .
	"first part : "
	index := 1.
	self firstCollection do: 
		[:each | 
		self assert: (self firstCollection at: index)=each.
		index := index+1.].
	"second part : "
	1 to: self secondCollection size do:
		[:i | 
		self assert: (self secondCollection at:i)= (result at:index).
		index:=index+1].
	"size : "
	self assert: result size = (self firstCollection size + self secondCollection size).
]

{ #category : #'tests - concatenation' }
TSequencedConcatenationTest >> testConcatenationWithEmpty [
	| result |
	result:= self empty,self secondCollection .
	
	1 to: self secondCollection size do:
		[:i | 
		self assert: (self secondCollection at:i)= (result at:i).
		].
	"size : "
	self assert: result size = ( self secondCollection size).
]

{ #category : #'tests - streaming' }
TSequencedConcatenationTest >> testNewStreamContentsReturnsCollectionOfCorrectSize [
	| result item originalSize |
	originalSize := 10.
	item := self firstCollection first.
	result := self collectionClass
		new: originalSize
		streamContents: [ :stream | 
			1 to: originalSize + 1 do: [ :each | stream nextPut: item ].
			stream position: originalSize ].
	self assert: result size equals: originalSize
]

{ #category : #'tests - streaming' }
TSequencedConcatenationTest >> testStreamContents [
	| result |
	result:= self collectionClass streamContents: [ :s|
		s 
			nextPutAll: self firstCollection;
			nextPutAll: self secondCollection ].
	self assert: result equals: self firstCollection, self secondCollection.
]

{ #category : #'tests - streaming' }
TSequencedConcatenationTest >> testStreamContentsProtocol [
	| result |
	result:= self collectionClass << [ :s|
		s 
			nextPutAll: self firstCollection;
			nextPutAll: self secondCollection ].
	self assert: result equals: self firstCollection, self secondCollection.
]

{ #category : #'tests - streaming' }
TSequencedConcatenationTest >> testStreamContentsSized [
	| result |
	result:= self collectionClass new: 1 streamContents: [ :s|
		s 
			nextPutAll: self firstCollection;
			nextPutAll: self secondCollection ].
	self assert: result equals: self firstCollection, self secondCollection.
	
	result:= self collectionClass new: 1000 streamContents: [ :s|
		s 
			nextPutAll: self firstCollection;
			nextPutAll: self secondCollection ].
	self assert: result equals: self firstCollection, self secondCollection.
]
