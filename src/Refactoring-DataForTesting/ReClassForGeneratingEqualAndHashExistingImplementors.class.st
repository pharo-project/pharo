Class {
	#name : 'ReClassForGeneratingEqualAndHashExistingImplementors',
	#superclass : 'Object',
	#instVars : [
		'name',
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
	^ self name = anObject name
]

{ #category : 'comparing' }
ReClassForGeneratingEqualAndHashExistingImplementors >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ self name hash
]

{ #category : 'accessing' }
ReClassForGeneratingEqualAndHashExistingImplementors >> iVarB [

	^ iVarB
]

{ #category : 'accessing' }
ReClassForGeneratingEqualAndHashExistingImplementors >> iVarB: anObject [

	iVarB := anObject
]

{ #category : 'accessing' }
ReClassForGeneratingEqualAndHashExistingImplementors >> name [

	^ name
]

{ #category : 'accessing' }
ReClassForGeneratingEqualAndHashExistingImplementors >> name: anObject [

	name := anObject
]
