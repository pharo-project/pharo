"
A test mock for pairs
"
Class {
	#name : #FLPair,
	#superclass : #Object,
	#instVars : [
		'left',
		'right'
	],
	#category : #'Fuel-Tests-Mocks'
}

{ #category : #comparing }
FLPair >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	^ self right = anObject right and: [ self left = anObject left ]
]

{ #category : #comparing }
FLPair >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ self right hash bitXor: self left hash
]

{ #category : #accessing }
FLPair >> left [
	^ left
]

{ #category : #accessing }
FLPair >> left: anObject [
	left := anObject
]

{ #category : #accessing }
FLPair >> method1 [
	self name.
	self printString.
	
]

{ #category : #accessing }
FLPair >> method2 [
	self name.
	
]

{ #category : #accessing }
FLPair >> methodWithTemp [
	| string |
	string := 'test'.
	self name.
	self printString.
	
]

{ #category : #printing }
FLPair >> printOn: aStream [
	"Append a sequence of characters to aStream that identify the receiver."

	super printOn: aStream.
	aStream
		nextPutAll: '(';
		print: left;
		space;
		print: right;
		nextPutAll: ')'
]

{ #category : #accessing }
FLPair >> right [
	^ right
]

{ #category : #accessing }
FLPair >> right: anObject [
	right := anObject
]
