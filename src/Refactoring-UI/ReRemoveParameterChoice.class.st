"
I am a breaking-change choice that proceeds with removing a parameter from a method.
"
Class {
	#name : 'ReRemoveParameterChoice',
	#superclass : 'ReChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'actions' }
ReRemoveParameterChoice >> action [

	driver removeParameter
]

{ #category : 'accessing' }
ReRemoveParameterChoice >> description [

	^ description ifNil: [ description := 'Remove the argument' ]
]
