Class {
	#name : 'ReClassForGeneratingEqualAndHashExistingImplementors',
	#superclass : 'Object',
	#instVars : [
		'iVarA',
		'iVarB'
	],
	#category : 'Refactoring-DataForTesting-ForGeneratingEqualAndHash',
	#package : 'Refactoring-DataForTesting',
	#tag : 'ForGeneratingEqualAndHash'
}

{ #category : 'comparing' }
ReClassForGeneratingEqualAndHashExistingImplementors >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject ifTrue: [ ^ true ].
	self class = anObject class ifFalse: [ ^ false ].
	^ iVarA = anObject iVarA
]

{ #category : 'comparing' }
ReClassForGeneratingEqualAndHashExistingImplementors >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ iVarA hash
]
