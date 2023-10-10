Class {
	#name : 'RBRemoveSharedVariableAndShowChoice',
	#superclass : 'RBVariableChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
RBRemoveSharedVariableAndShowChoice >> action [

	driver browseSharedVariableReferences.
	driver applyChanges
]

{ #category : 'accessing' }
RBRemoveSharedVariableAndShowChoice >> description [ 

	^ 'Remove shared variable(s) and browse left user(s)'
]
