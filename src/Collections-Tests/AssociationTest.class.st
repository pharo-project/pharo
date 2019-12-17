"
SUnit tests for Associations
"
Class {
	#name : #AssociationTest,
	#superclass : #ClassTestCase,
	#instVars : [
		'a',
		'b'
	],
	#category : #'Collections-Tests-Support'
}

{ #category : #running }
AssociationTest >> setUp [

	super setUp.
	a := 1 -> 'one'.
	b := 1 -> 'een'
]

{ #category : #tests }
AssociationTest >> testComparison [

	self assert: ((#a -> 'foo') < (#b -> 'zork'))
]

{ #category : #tests }
AssociationTest >> testEquality [
	self
		assert: a key equals: b key;
		deny: a value equals: b value;
		deny: a equals: b
]

{ #category : #tests }
AssociationTest >> testHash [

	self 
		assert: a hash equals: a copy hash;
		assert: a hash equals: b hash
]

{ #category : #tests }
AssociationTest >> testIsSelfEvaluating [
	| anotherAssociation |
	self assert: a isSelfEvaluating.
	
	self assert: a printString equals: '1->''one'''.
	
	anotherAssociation := Object new -> Object new.
	anotherAssociation isSelfEvaluating 
		ifTrue: [ self assert: (self class compiler evaluate: anotherAssociation printString) = anotherAssociation
							description: 'a self evaluating should evaluate as self' ]

]

{ #category : #tests }
AssociationTest >> testStoreOnNegativeInteger [
	| association storeString |
	association := 'a'-> -1.
	
	storeString := String streamContents: [ :s | association storeOn: s ].
	
	self assert: storeString equals: '(''a''-> -1)'
]

{ #category : #tests }
AssociationTest >> testStoreOnPositiveInteger [
	| association storeString |
	association := 'a'-> 1.
	storeString := String streamContents: [ :s | association storeOn: s ].
	
	self assert: storeString equals: '(''a''->1)'
]
