Class {
	#name : #ClyScopeExample,
	#superclass : #ClyTypedScope,
	#instVars : [
		'extraParameter'
	],
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #comparing }
ClyScopeExample >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject ifTrue: [ ^ true ].
	super = anObject ifFalse: [ ^ false ].
	^ extraParameter = anObject extraParameter
]

{ #category : #accessing }
ClyScopeExample >> extraParameter [
	^ extraParameter
]

{ #category : #accessing }
ClyScopeExample >> extraParameter: anObject [
	extraParameter := anObject
]

{ #category : #comparing }
ClyScopeExample >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^super hash bitXor: extraParameter hash
]
