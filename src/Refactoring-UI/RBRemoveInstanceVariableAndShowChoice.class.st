Class {
	#name : 'RBRemoveInstanceVariableAndShowChoice',
	#superclass : 'RBInstanceVariableChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
RBRemoveInstanceVariableAndShowChoice >> action [

	driver browseInstanceVariableReferences.
	driver removeInstanceVariables
]

{ #category : 'accessing' }
RBRemoveInstanceVariableAndShowChoice >> description [ 

	^ 'Remove instance variable(s) and browse left user(s)'
]
