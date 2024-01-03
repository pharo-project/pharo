Class {
	#name : 'ReRemoveChoice',
	#superclass : 'ReRemoveMethodChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'execution' }
ReRemoveChoice >> action [

	driver applyChanges
]

{ #category : 'accessing' }
ReRemoveChoice >> description [

	^ 'Remove it'
]
