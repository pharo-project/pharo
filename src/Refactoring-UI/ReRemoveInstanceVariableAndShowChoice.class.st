Class {
	#name : 'ReRemoveInstanceVariableAndShowChoice',
	#superclass : 'ReVariableChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
ReRemoveInstanceVariableAndShowChoice >> action [

	driver browseInstanceVariableReferences.
	driver applyChanges
]

{ #category : 'accessing' }
ReRemoveInstanceVariableAndShowChoice >> description [ 

	^ 'Remove instance variable(s) and browse left user(s)'
]
