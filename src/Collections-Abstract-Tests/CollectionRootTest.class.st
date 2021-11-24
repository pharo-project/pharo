"
I'm the root of the hierarchy of the collection tests.

"
Class {
	#name : #CollectionRootTest,
	#superclass : #ClassTestCase,
	#traits : 'TIterateTest + TEmptyTest + TSizeTest',
	#classTraits : 'TIterateTest classTrait + TEmptyTest classTrait + TSizeTest classTrait',
	#category : #'Collections-Abstract-Tests-Base'
}

{ #category : #testing }
CollectionRootTest class >> isAbstract [

	^ self name = #CollectionRootTest
]

{ #category : #requirements }
CollectionRootTest >> collectionWithoutNilElements [
" return a collection that doesn't includes a nil element  and that doesn't includes equal elements'"
	self subclassResponsibility
]

{ #category : #requirements }
CollectionRootTest >> doWithoutNumber [

	^ 2
]

{ #category : #requirements }
CollectionRootTest >> element [
	^ 3
]

{ #category : #requirements }
CollectionRootTest >> elementTwiceIn [
	^ 1 "12332312322"
]

{ #category : #requirements }
CollectionRootTest >> empty [
	self subclassResponsibility
]

{ #category : #requirements }
CollectionRootTest >> expectedElementByDetect [

	^ -2
]

{ #category : #requirements }
CollectionRootTest >> nonEmpty [

	self subclassResponsibility
]

{ #category : #requirements }
CollectionRootTest >> sizeCollection [
	"Answers a collection not empty"
	^ self subclassResponsibility
]

{ #category : #'tests - iterating' }
CollectionRootTest >> testAllSatisfyEmpty [

	self assert: ( self empty allSatisfy: [:each | false]).
]

{ #category : #'tests - iterating' }
CollectionRootTest >> testAnySastify [

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
CollectionRootTest >> testBasicCollect [

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
CollectionRootTest >> testBasicCollectThenDo [

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
CollectionRootTest >> testDetect [

	| res element |
	element := self collectionWithoutNilElements anyOne .
	 
	res := self collectionWithoutNilElements  detect: [:each | each = element].
	self assert: (res  = element).
]

{ #category : #'tests - iterate' }
CollectionRootTest >> testDetectIfFoundIfNoneWhenNobodyIsFound [
	| wasFound |
	wasFound := self collectionWithoutNilElements detect: [ :each | each isNil ] ifFound: [ true ] ifNone: [ false ].
	self deny: wasFound
]

{ #category : #'tests - iterate' }
CollectionRootTest >> testDetectIfFoundIfNoneWhenSomethingIsFound [
	| wasFound foundObject |
	foundObject := nil.
	wasFound := self collectionWithoutNilElements
		detect: [ :each | each notNil ]
		ifFound: [ :element | 
			foundObject := element.
			true ]
		ifNone: [ false ].
	self
		assert: wasFound;
		assert: (self collectionWithoutNilElements includes: foundObject)
]

{ #category : #'tests - iterate' }
CollectionRootTest >> testDetectIfFoundIfNoneWhenSomethingIsFoundIgnoringTheFoundObject [
	"The foundBlock can be a zero argument block ignoring the object found"

	| wasFound |
	wasFound := self collectionWithoutNilElements detect: [ :each | each notNil ] ifFound: [ true ] ifNone: [ false ].
	self assert: wasFound
]

{ #category : #'tests - iterate' }
CollectionRootTest >> testDetectIfFoundWhenNobodyIsFound [
	| wasFound |
	wasFound := false.
	self collectionWithoutNilElements detect: [ :each | each isNil ] ifFound: [ wasFound := true ].
	self deny: wasFound
]

{ #category : #'tests - iterate' }
CollectionRootTest >> testDetectIfFoundWhenSomethingIsFound [
	| wasFound foundObject |
	wasFound := false.
	foundObject := nil.
	self collectionWithoutNilElements
		detect: [ :each | each notNil ]
		ifFound: [ :element | 
			foundObject := element.
			wasFound := true ].
	self
		assert: wasFound;
		assert: (self collectionWithoutNilElements includes: foundObject)
]

{ #category : #'tests - iterate' }
CollectionRootTest >> testDetectIfFoundWhenSomethingIsFoundIgnoringTheFoundObject [
	"The foundBlock can be a zero argument block ignoring the object found"

	| wasFound |
	wasFound := false.
	self collectionWithoutNilElements detect: [ :each | each notNil ] ifFound: [ wasFound := true ].
	self assert: wasFound
]

{ #category : #'tests - iterating' }
CollectionRootTest >> testDetectIfNone [

	| res element |
	res := self collectionWithoutNilElements  detect: [:each | each notNil not] ifNone: [100].
	self assert: res  = 100.
	
	element := self collectionWithoutNilElements anyOne.
	res := self collectionWithoutNilElements  detect: [:each | each = element] ifNone: [100].
	self assert: res  = element.
]

{ #category : #'tests - iterating' }
CollectionRootTest >> testDo2 [
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
CollectionRootTest >> testDoWithout [
	
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
CollectionRootTest >> testNoneSatisfyEmpty [

	self assert: ( self empty noneSatisfy: [:each | false]).
]

{ #category : #'tests - iterating' }
CollectionRootTest >> testReject [

	| res element |
	res := self collectionWithoutNilElements reject: [:each | each notNil not].
	self assert: res size = self collectionWithoutNilElements size.
	
	element := self collectionWithoutNilElements anyOne.
	res := self collectionWithoutNilElements  reject: [:each | each = element].
	self assert: res size = (self collectionWithoutNilElements size - 1).
]

{ #category : #'tests - iterating' }
CollectionRootTest >> testRejectEmpty [

	| res |
	res := self empty reject: [:each | each odd].
	self assert: res size = self empty size
]

{ #category : #'tests - iterate' }
CollectionRootTest >> testRejectNoReject [
	| res collection |
	collection := self collectionWithoutNilElements.
	res := collection reject: [ :each | each isNil ].
	self assert: res size equals: collection size
]

{ #category : #'tests - iterating' }
CollectionRootTest >> testRejectThenCollect [

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
CollectionRootTest >> testRejectThenDo [

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
CollectionRootTest >> testSelect [

	| result element |
	result := self collectionWithoutNilElements select: [ :each | each notNil].
	self assert: result size equals: self collectionWithoutNilElements size.
	
	element := self collectionWithoutNilElements anyOne.
	result := self collectionWithoutNilElements select: [ :each | (each = element) not].
	self assert: result size equals: (self collectionWithoutNilElements size - 1).
]

{ #category : #'tests - iterating' }
CollectionRootTest >> testSelectThenCollect [

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
CollectionRootTest >> testSelectThenDo [

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
