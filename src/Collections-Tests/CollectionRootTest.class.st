"
I'm the root of the hierarchy of the collection tests.

"
Class {
	#name : #CollectionRootTest,
	#superclass : #ClassTestCase,
	#traits : 'TIterateTest + TEmptyTest + TSizeTest',
	#classTraits : 'TIterateTest classTrait + TEmptyTest classTrait + TSizeTest classTrait',
	#category : #'Collections-Tests-Abstract'
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

{ #category : #'tests - iterate' }
CollectionRootTest >> testRejectNoReject [
	| res collection |
	collection := self collectionWithoutNilElements.
	res := collection reject: [ :each | each isNil ].
	self assert: res size equals: collection size
]
