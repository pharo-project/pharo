Class {
	#name : 'ReRemoveSharedVariableAndShowChoice',
	#superclass : 'ReVariableChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
ReRemoveSharedVariableAndShowChoice >> action [

	driver browseSharedVariableReferences.
	driver applyChanges
]

{ #category : 'accessing' }
ReRemoveSharedVariableAndShowChoice >> description [ 

	^ 'Remove shared variable(s) and browse left user(s)'
]
