Class {
	#name : 'ReRemoveInstanceVariableChoice',
	#superclass : 'ReVariableChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
ReRemoveInstanceVariableChoice >> action [

	driver applyChanges
]

{ #category : 'accessing' }
ReRemoveInstanceVariableChoice >> description [ 

	^ 'Remove instance variables anyway'
]
