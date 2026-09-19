"
I am a breaking-change choice that proceeds with adding a parameter to a method.
"
Class {
	#name : 'ReAddParameterChoice',
	#superclass : 'ReChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'actions' }
ReAddParameterChoice >> action [

	driver addParameter
]

{ #category : 'accessing' }
ReAddParameterChoice >> description [

	^ description ifNil: [ description := 'Add the argument' ]
]
