"
I'm the root of the hierarchy of the collection tests.

"
Class {
	#name : 'CollectionRootTest',
	#superclass : 'ClassTestCase',
	#traits : 'TIterateTest + TEmptyTest + TSizeTest',
	#classTraits : 'TIterateTest classTrait + TEmptyTest classTrait + TSizeTest classTrait',
	#category : 'Collections-Abstract-Tests-Base',
	#package : 'Collections-Abstract-Tests',
	#tag : 'Base'
}

{ #category : 'testing' }
CollectionRootTest class >> isAbstract [

	^ self name = #CollectionRootTest
]

{ #category : 'requirements' }
CollectionRootTest >> collectionWithoutNilElements [
" return a collection that doesn't includes a nil element  and that doesn't includes equal elements'"
	self subclassResponsibility
]

{ #category : 'requirements' }
CollectionRootTest >> doWithoutNumber [

	^ 2
]

{ #category : 'requirements' }
CollectionRootTest >> element [
	^ 3
]

{ #category : 'requirements' }
CollectionRootTest >> elementTwiceIn [
	^ 1 "12332312322"
]

{ #category : 'requirements' }
CollectionRootTest >> empty [
	self subclassResponsibility
]

{ #category : 'requirements' }
CollectionRootTest >> expectedElementByDetect [

	^ -2
]

{ #category : 'requirements' }
CollectionRootTest >> nonEmpty [

	self subclassResponsibility
]

{ #category : 'requirements' }
CollectionRootTest >> sizeCollection [
	"Answers a collection not empty"
	^ self subclassResponsibility
]

{ #category : 'tests - converting' }
CollectionRootTest >> testAsMultilineString [

	| expectedResult |
	expectedResult := 'a
b
c'.

	self
		assert: #(a b c) asMultilineString equals: expectedResult;
		assert: 'abc' asMultilineString equals: expectedResult
]

{ #category : 'tests - copying' }
CollectionRootTest >> testCopyWithAll [

	| col res |

	self assert: ('Hello' copyWithAll: 'World') equals: 'HelloWorld'.

	col := #( a b ).
	res := col copyWithAll: #( c d ).

	self
		assert: res equals: #( a b c d );
		deny: col identicalTo: res.

	col := Set with: #a with: #b.
	res := col copyWithAll: #( c d ).

	self
		assert: res size equals: 4;
		assert: (res includes: #a);
		assert: (res includes: #b);
		assert: (res includes: #c);
		assert: (res includes: #d);
		deny: col identicalTo: res.

	col := OrderedCollection with: #a with: #b.
	res := col copyWithAll: #( c d ).
	self
		assert: res equals: (OrderedCollection with: #a with: #b with: #c with: #d);
		deny: col identicalTo: res
]

{ #category : 'tests - copying' }
CollectionRootTest >> testCopyWithoutFirst [

	| col res |

	self assert: ('HelloWorld' copyWithoutFirst: $o) equals: 'HellWorld'.

	col := #( a b a c ).
	res := col copyWithoutFirst: #b.

	self
		assert: res equals: #( a a c );
		deny: col identicalTo: res.

	col := Set with: #a with: #b with: #c.
	res := col copyWithoutFirst: #a.

	self
		assert: res size equals: 2;
		assert: (res includes: #b);
		assert: (res includes: #c);
		deny: col identicalTo: res
]

{ #category : 'tests - copying' }
CollectionRootTest >> testCopyWithoutFirstOccuranceOf [

	| col res |

	self assert: ('HelloWorld' copyWithoutFirstOccurrenceOf: $o) equals: 'HellWorld'.

	col := #( a b a c ).
	res := col copyWithoutFirstOccurrenceOf: #a.

	self
		assert: res equals: #( b a c );
		deny: col identicalTo: res.

	col := #( a b a c ).
	res := col copyWithoutFirstOccurrenceOf: #b.
	self
		assert: res equals: #( a a c );
		deny: col identicalTo: res.

	col := Set with: #a with: #b with: #c.
	res := col copyWithoutFirstOccurrenceOf: #a.

	self
		assert: res size equals: 2;
		assert: (res includes: #b);
		assert: (res includes: #c);
		deny: col identicalTo: res
]

{ #category : 'tests - iterate' }
CollectionRootTest >> testDetectIfFoundIfNoneWhenNobodyIsFound [
	| wasFound |
	wasFound := self collectionWithoutNilElements detect: [ :each | each isNil ] ifFound: [ true ] ifNone: [ false ].
	self deny: wasFound
]

{ #category : 'tests - iterate' }
CollectionRootTest >> testDetectIfFoundIfNoneWhenSomethingIsFound [
	| wasFound foundObject |
	foundObject := nil.
	wasFound := self collectionWithoutNilElements
		detect: [ :each | each isNotNil ]
		ifFound: [ :element |
			foundObject := element.
			true ]
		ifNone: [ false ].
	self
		assert: wasFound;
		assert: (self collectionWithoutNilElements includes: foundObject)
]

{ #category : 'tests - iterate' }
CollectionRootTest >> testDetectIfFoundIfNoneWhenSomethingIsFoundIgnoringTheFoundObject [
	"The foundBlock can be a zero argument block ignoring the object found"

	| wasFound |
	wasFound := self collectionWithoutNilElements detect: [ :each | each isNotNil ] ifFound: [ true ] ifNone: [ false ].
	self assert: wasFound
]

{ #category : 'tests - iterate' }
CollectionRootTest >> testDetectIfFoundWhenNobodyIsFound [
	| wasFound |
	wasFound := false.
	self collectionWithoutNilElements detect: [ :each | each isNil ] ifFound: [ wasFound := true ].
	self deny: wasFound
]

{ #category : 'tests - iterate' }
CollectionRootTest >> testDetectIfFoundWhenSomethingIsFound [
	| wasFound foundObject |
	wasFound := false.
	foundObject := nil.
	self collectionWithoutNilElements
		detect: [ :each | each isNotNil ]
		ifFound: [ :element |
			foundObject := element.
			wasFound := true ].
	self
		assert: wasFound;
		assert: (self collectionWithoutNilElements includes: foundObject)
]

{ #category : 'tests - iterate' }
CollectionRootTest >> testDetectIfFoundWhenSomethingIsFoundIgnoringTheFoundObject [
	"The foundBlock can be a zero argument block ignoring the object found"

	| wasFound |
	wasFound := false.
	self collectionWithoutNilElements detect: [ :each | each isNotNil ] ifFound: [ wasFound := true ].
	self assert: wasFound
]

{ #category : 'tests - iterate' }
CollectionRootTest >> testRejectNoReject [
	| res collection |
	collection := self collectionWithoutNilElements.
	res := collection reject: [ :each | each isNil ].
	self assert: res size equals: collection size
]
