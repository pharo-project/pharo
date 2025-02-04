Class {
	#name : 'RePushUpInstanceVariableChoice',
	#superclass : 'ReVariableChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
RePushUpInstanceVariableChoice >> action [
	driver pullUpReferencedInstVars
]

{ #category : 'accessing' }
RePushUpInstanceVariableChoice >> description [ 

	^ 'Push up instance variable(s)'
]
