"
FLPerson is a class just to be able to change the hash of an object and try to test problems with hash.
"
Class {
	#name : #FLPerson,
	#superclass : #Object,
	#instVars : [
		'id'
	],
	#category : 'Fuel-Tests-Core-Mocks'
}

{ #category : #comparing }
FLPerson >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	^ id = anObject id
]

{ #category : #comparing }
FLPerson >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ id
]

{ #category : #accessing }
FLPerson >> id [
	^ id
]

{ #category : #accessing }
FLPerson >> id: anObject [
	id := anObject
]
