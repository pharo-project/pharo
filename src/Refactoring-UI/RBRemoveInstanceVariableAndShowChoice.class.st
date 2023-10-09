Class {
	#name : 'RBRemoveInstanceVariableAndShowChoice',
	#superclass : 'RBVariableChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
RBRemoveInstanceVariableAndShowChoice >> action [

	driver browseInstanceVariableReferences.
	driver applyChanges
]

{ #category : 'accessing' }
RBRemoveInstanceVariableAndShowChoice >> description [ 

	^ 'Remove instance variable(s) and browse left user(s)'
]
