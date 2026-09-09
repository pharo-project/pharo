Class {
	#name : 'ReNewVariableNameChoice',
	#superclass : 'ReVariableChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
ReNewVariableNameChoice >> action [

	driver runRefactoring
]

{ #category : 'accessing' }
ReNewVariableNameChoice >> description [

	^ 'Pick a new temporary variable name'
]
