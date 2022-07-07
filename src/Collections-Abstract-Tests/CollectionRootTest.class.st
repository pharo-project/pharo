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
		" #do doesn't iterate with the same objects than those in the collection for Float32Array( I don' t know why ) . That's why I use #includes: and not #identityIncludes:  '"
		(collection includes: each) ifTrue: [
			" the collection used doesn't include equal elements. Therefore each element viewed should not have been viewed before "
			( cptElementsIn includes: each ) ifFalse: [ cptElementsIn add: each ] .
			]. 
		].
	self assert: cptElementsViewed = collection size.
	self assert: cptElementsIn size  = collection size.
]

{ #category : #'tests - iterate' }
CollectionRootTest >> testRejectNoReject [
	| res collection |
	collection := self collectionWithoutNilElements.
	res := collection reject: [ :each | each isNil ].
	self assert: res size equals: collection size
]
