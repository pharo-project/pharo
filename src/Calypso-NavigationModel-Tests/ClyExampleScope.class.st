Class {
	#name : #ClyExampleScope,
	#superclass : #ClyTypedScope,
	#instVars : [
		'extraParameter'
	],
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #comparing }
ClyExampleScope >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject ifTrue: [ ^ true ].
	super = anObject ifFalse: [ ^ false ].
	^ extraParameter = anObject extraParameter
]

{ #category : #accessing }
ClyExampleScope >> extraParameter [
	^ extraParameter
]

{ #category : #accessing }
ClyExampleScope >> extraParameter: anObject [
	extraParameter := anObject
]

{ #category : #comparing }
ClyExampleScope >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^super hash bitXor: extraParameter hash
]
