Class {
	#name : 'ReRemoveSharedVariableChoice',
	#superclass : 'ReVariableChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
ReRemoveSharedVariableChoice >> action [

	driver applyChanges
]

{ #category : 'accessing' }
ReRemoveSharedVariableChoice >> description [ 

	^ 'Remove shared variables anyway'
]
