Class {
	#name : 'RBRemoveInstanceVariableChoice',
	#superclass : 'RBInstanceVariableChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
RBRemoveInstanceVariableChoice >> action [

	driver removeInstanceVariables
]

{ #category : 'accessing' }
RBRemoveInstanceVariableChoice >> description [ 

	^ 'Remove instance variables anyway'
]
