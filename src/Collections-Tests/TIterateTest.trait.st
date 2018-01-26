"
I'm testing that high-order iterators are working.
Note however that I do not pay attention to the order of the elements.
Hence, my doTest is designed to not checking that. 
TIterateSequenceableTest is about this distinction and as such will not be applicable to test
Set, Bag, Dictionary. 



"
Trait {
	#name : #TIterateTest,
	#category : #'Collections-Tests-Abstract'
}

{ #category : #requirements }
TIterateTest >> collectionWithoutNilElements [
" return a collection that doesn't includes a nil element  and that doesn't includes equal elements'"
	self explicitRequirement
]

{ #category : #requirements }
TIterateTest >> empty [

	self explicitRequirement
]

{ #category : #'tests - fixture' }
TIterateTest >> test0FixtureIterateTest [
	| res |
	self collectionWithoutNilElements.
	self assert: (self collectionWithoutNilElements occurrencesOf: nil) = 0.
	res := true.
	self collectionWithoutNilElements
		detect: [ :each | (self collectionWithoutNilElements occurrencesOf: each) > 1 ]
		ifNone: [ res := false ].
	self assert: res = false
]

{ #category : #'tests - iterating' }
TIterateTest >> testAllSatisfy [

	| element |
	" when all element  satisfy the condition, should return true : "
	self assert: ( self collectionWithoutNilElements  allSatisfy: [:each | (each notNil) ] ).
	
	" when all element don't satisfy the condition, should return false : "
	self deny: ( self collectionWithoutNilElements  allSatisfy: [:each | (each notNil) not ] ).
	
	" when only one element doesn't satisfy the condition' should return false'"
	element := self collectionWithoutNilElements anyOne.
	self deny: ( self collectionWithoutNilElements  allSatisfy: [:each | (each = element) not] ).
]

{ #category : #'tests - iterating' }
TIterateTest >> testAllSatisfyEmpty [

	self assert: ( self empty allSatisfy: [:each | false]).
	
]

{ #category : #'tests - iterating' }
TIterateTest >> testAnySastify [

	| element |
	" when all elements satisty the condition, should return true :"
	self assert: ( self collectionWithoutNilElements anySatisfy: [:each | each notNil ]).
	
	" when only one element satisfy the condition, should return true :"
	element := self collectionWithoutNilElements anyOne.
	self assert: ( self collectionWithoutNilElements  anySatisfy: [:each | (each = element)  ]   ).
	
	" when all elements don't satisty the condition, should return false :"
	self deny: ( self collectionWithoutNilElements anySatisfy: [:each | (each notNil) not ]).

]

{ #category : #'tests - iterating' }
TIterateTest >> testBasicCollect [

	| res index |
	index := 0.
	res := self collectionWithoutNilElements collect: [ :each | 
		index := index + 1.
		each ].
	
	res do: [ :each | 
		self assert: (self collectionWithoutNilElements occurrencesOf: each) = (res occurrencesOf: each)].
	self assert: index equals: self collectionWithoutNilElements size.
	 
]

{ #category : #'tests - iterating' }
TIterateTest >> testBasicCollectEmpty [

	| res |
	res := self empty collect: [:each | each class].
	self assert: res isEmpty
	
]

{ #category : #'tests - iterating' }
TIterateTest >> testBasicCollectThenDo [

	| result index |
	index := 0.
	result := self collectionWithoutNilElements 
		collect: [ :each | nil ]
		thenDo: [ :each | 
			self assert: each isNil.
			index := index + 1]	.
			
	self assert: result equals: self collectionWithoutNilElements.
	self assert: index equals: self collectionWithoutNilElements size.
	 
]

{ #category : #'tests - iterating' }
TIterateTest >> testCollectOnEmpty [
	self assert: (self empty collect: [:e | self fail]) isEmpty
]

{ #category : #'tests - iterating' }
TIterateTest >> testCollectThenDoOnEmpty [

	self assert: (self empty collect: [:e | self fail] thenDo: [ self fail ]) isEmpty
]

{ #category : #'tests - iterating' }
TIterateTest >> testCollectThenSelectOnEmpty [

	self assert: (self empty collect: [:e | self fail] thenSelect: [:e | self fail ]) isEmpty
]

{ #category : #'tests - iterating' }
TIterateTest >> testDetect [

	| res element |
	element := self collectionWithoutNilElements anyOne .
	 
	res := self collectionWithoutNilElements  detect: [:each | each = element].
	self assert: (res  = element).

	
	
]

{ #category : #'tests - iterating' }
TIterateTest >> testDetectIfNone [

	| res element |
	res := self collectionWithoutNilElements  detect: [:each | each notNil not] ifNone: [100].
	self assert: res  = 100.
	
	element := self collectionWithoutNilElements anyOne.
	res := self collectionWithoutNilElements  detect: [:each | each = element] ifNone: [100].
	self assert: res  = element.

	
	
]

{ #category : #'tests - iterating' }
TIterateTest >> testDo2 [
	"dc: Bad test, it assumes that a new instance of #speciesClass allows addition with #add:. This is not the case of Interval for which species is Array."
	"res := self speciesClass new.  
	self collection do: [:each | res add: each class].
	self assert: res = self result. "
	| collection cptElementsViewed cptElementsIn |
	collection := self collectionWithoutNilElements.
	cptElementsViewed := 0.
	cptElementsIn := OrderedCollection new.
	collection do: 
		[ :each | 
		cptElementsViewed := cptElementsViewed + 1.
		" #do doesn't iterate with the same objects than those in the collection for FloatArray( I don' t know why ) . That's why I use #includes: and not #identityIncludes:  '"
		(collection includes: each) ifTrue: [
			" the collection used doesn't include equal elements. Therefore each element viewed should not have been viewed before "
			( cptElementsIn includes: each ) ifFalse: [ cptElementsIn add: each ] .
			]. 
		].
	self assert: cptElementsViewed = collection size.
	self assert: cptElementsIn size  = collection size.
	
	
]

{ #category : #'tests - iterating' }
TIterateTest >> testDoSeparatedBy [
	| string expectedString beforeFirst |
	
	string := ''.
	self collectionWithoutNilElements  
		do: [ :each | string := string , each asString ]
		separatedBy: [ string := string , '|' ].
		
	expectedString := ''.
	beforeFirst := true.
	self collectionWithoutNilElements  do: 
		[ :each | 
		beforeFirst = true 
			ifTrue: [ beforeFirst := false ]
			ifFalse: [ expectedString := expectedString , '|' ].
		expectedString := expectedString , each asString ].
	self assert: expectedString = string
]

{ #category : #'tests - iterating' }
TIterateTest >> testDoWithout [
	
	| res element collection |
	collection := self collectionWithoutNilElements .	
	res := OrderedCollection new.  
	element := self collectionWithoutNilElements anyOne .
	collection  do: [:each | res add: each] without: element  .
	" verifying result :"
	self assert: res size = (collection  size - (collection  occurrencesOf: element)).
	res do: [:each | self assert: (collection occurrencesOf: each) = ( res occurrencesOf: each ) ].
	
]

{ #category : #'tests - iterating' }
TIterateTest >> testInjectInto [
	|result|
	result:= self collectionWithoutNilElements 
		inject: 0
		into: [:inj :ele | ele notNil ifTrue: [ inj + 1 ]].
	
	self assert: self collectionWithoutNilElements size = result .
]

{ #category : #'tests - iterating' }
TIterateTest >> testNoneSatisfy [

	| element |
	self assert: ( self collectionWithoutNilElements  noneSatisfy: [:each | each notNil not ] ).
	element := self collectionWithoutNilElements anyOne.
	self deny: ( self collectionWithoutNilElements  noneSatisfy: [:each | (each = element)not ] ).
]

{ #category : #'tests - iterating' }
TIterateTest >> testNoneSatisfyEmpty [

	self assert: ( self empty noneSatisfy: [:each | false]).
	
]

{ #category : #'tests - iterating' }
TIterateTest >> testReject [

	| res element |
	res := self collectionWithoutNilElements reject: [:each | each notNil not].
	self assert: res size = self collectionWithoutNilElements size.
	
	element := self collectionWithoutNilElements anyOne.
	res := self collectionWithoutNilElements  reject: [:each | each = element].
	self assert: res size = (self collectionWithoutNilElements size - 1).
	
	
	
]

{ #category : #'tests - iterating' }
TIterateTest >> testRejectAllThenCollect [
	| result |
	
	result := self collectionWithoutNilElements 
		reject: [ :each | each notNil ] 
		thenCollect: [ :each| self fail ].
	
	self assert: result isEmpty
]

{ #category : #'tests - iterating' }
TIterateTest >> testRejectAllThenDo [
	| result |
	
	result := self collectionWithoutNilElements 
		reject: [ :each | each notNil ] 
		thenDo: [ :each | self fail ].
	
	self assert: result equals: self collectionWithoutNilElements
]

{ #category : #'tests - iterating' }
TIterateTest >> testRejectEmpty [

	| res |
	res := self empty reject: [:each | each odd].
	self assert: res size = self empty size
	
]

{ #category : #'tests - iterating' }
TIterateTest >> testRejectNoReject [

	| res |
	res := self collectionWithoutNilElements  reject: [:each | each notNil not].
	self assert: res size = self collectionWithoutNilElements size.
	
]

{ #category : #'tests - iterating' }
TIterateTest >> testRejectThenCollect [

	| result index selectIndex pivot |
	
	index := 0.
	selectIndex := 0.
	pivot := self collectionWithoutNilElements anyOne.
	
	result := self collectionWithoutNilElements 
		reject: [ :each | 
			selectIndex := selectIndex + 1.
			"reject the first element"
			selectIndex = 1 ]
		thenCollect: [ :each | 
			self assert: each notNil.
			index := index + 1.
			pivot ].
			
	self assert: result ~= self collectionWithoutNilElements.
	self assert: selectIndex equals: self collectionWithoutNilElements size.
	self assert: index equals: self collectionWithoutNilElements size - 1.
	
	self assert: (self collectionWithoutNilElements occurrencesOf: pivot) equals: 1.
	"should be > 1 for standard collection and = 1 for those that do not allow exact duplicates"
	self assert: (result occurrencesOf: pivot) >= 1.
	 
]

{ #category : #'tests - iterating' }
TIterateTest >> testRejectThenCollectEmpty [

	self assert: (self empty reject: [:e | self fail ] thenCollect: [ :each| self fail ]) isEmpty
]

{ #category : #'tests - iterating' }
TIterateTest >> testRejectThenDo [

	| result index rejectIndex |
	
	index := 0.
	rejectIndex := 0.
	
	result := self collectionWithoutNilElements 
		reject: [ :each | 
			rejectIndex := rejectIndex + 1.
			"reject the first element"
			rejectIndex = 1 ]
		thenDo: [ :each | 
			self assert: each notNil.
			index := index + 1]	.
			
	self assert: result equals: self collectionWithoutNilElements.
	self assert: rejectIndex equals: self collectionWithoutNilElements size.
	self assert: index equals: self collectionWithoutNilElements size - 1.
	 
]

{ #category : #'tests - iterating' }
TIterateTest >> testRejectThenDoOnEmpty [

	self assert: (self empty reject: [:e | self fail ] thenDo: [ self fail ]) isEmpty
]

{ #category : #'tests - iterating' }
TIterateTest >> testSelect [

	| result element |
	result := self collectionWithoutNilElements select: [ :each | each notNil].
	self assert: result size equals: self collectionWithoutNilElements size.
	
	element := self collectionWithoutNilElements anyOne.
	result := self collectionWithoutNilElements select: [ :each | (each = element) not].
	self assert: result size equals: (self collectionWithoutNilElements size - 1).
	
]

{ #category : #'tests - iterating' }
TIterateTest >> testSelectNoneThenCollect [
	| result |
	
	result := self collectionWithoutNilElements 
		select: [ :each | each isNil ] 
		thenCollect: [ :each| self fail ].
	
	self assert: result isEmpty
]

{ #category : #'tests - iterating' }
TIterateTest >> testSelectNoneThenDo [
	| result |
	
	result := self collectionWithoutNilElements 
		select: [ :each | each isNil ] 
		thenDo: [ self fail ].
	
	self assert: result equals: self collectionWithoutNilElements
]

{ #category : #'tests - iterating' }
TIterateTest >> testSelectOnEmpty [

	self assert: (self empty select: [:e | self fail]) isEmpty
	
]

{ #category : #'tests - iterating' }
TIterateTest >> testSelectThenCollect [

	| result index selectIndex pivot |
	
	index := 0.
	selectIndex := 0.
	pivot := self collectionWithoutNilElements anyOne.
	
	result := self collectionWithoutNilElements 
		select: [ :each | 
			selectIndex := selectIndex + 1.
			"reject the first element"
			selectIndex > 1 ]
		thenCollect: [ :each | 
			self assert: each notNil.
			index := index + 1.
			pivot ].
			
	self assert: result ~= self collectionWithoutNilElements.
	self assert: selectIndex equals: self collectionWithoutNilElements size.
	self assert: index equals: self collectionWithoutNilElements size - 1.
	
	self assert: (self collectionWithoutNilElements occurrencesOf: pivot) equals: 1.
	"should be > 1 for standard collection and = 1 for those that do not allow exact duplicates"
	self assert: (result occurrencesOf: pivot) >= 1.
	 
]

{ #category : #'tests - iterating' }
TIterateTest >> testSelectThenCollectOnEmpty [

	self assert: (self empty select: [:e | self fail ] thenCollect: [ self fail ]) isEmpty
]

{ #category : #'tests - iterating' }
TIterateTest >> testSelectThenDo [

	| result index selectIndex |
	
	index := 0.
	selectIndex := 0.
	
	result := self collectionWithoutNilElements 
		select: [ :each | 
			selectIndex := selectIndex + 1.
			"reject the first element"
			selectIndex > 1 ]
		thenDo: [ :each | 
			self assert: each notNil.
			index := index + 1]	.
			
	self assert: result equals: self collectionWithoutNilElements.
	self assert: selectIndex equals: self collectionWithoutNilElements size.
	self assert: index equals: self collectionWithoutNilElements size - 1.
	 
]

{ #category : #'tests - iterating' }
TIterateTest >> testSelectThenDoOnEmpty [

	self assert: (self empty select: [:e | self fail ] thenDo: [ self fail ]) isEmpty
]

{ #category : #'tests - iterating' }
TIterateTest >> testSumNumbers [
	|result|
	result:= self collectionWithoutNilElements 
		sumNumbers: [ :ele | ele notNil ifTrue: [ 1 ] ifFalse: [ 0 ]].
	
	self assert: self collectionWithoutNilElements size = result
]
