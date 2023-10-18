Class {
	#name : 'RBRemoveSharedVariableChoice',
	#superclass : 'RBVariableChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
RBRemoveSharedVariableChoice >> action [

	driver applyChanges
]

{ #category : 'accessing' }
RBRemoveSharedVariableChoice >> description [ 

	^ 'Remove shared variables anyway'
]
