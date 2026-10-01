Class {
	#name : 'ReMoveClasstoClassSideChoice',
	#superclass : 'ReMethodChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
ReMoveClasstoClassSideChoice >> action [

	driver applyChanges
]

{ #category : 'accessing' }
ReMoveClasstoClassSideChoice >> description [

	^ 'There is a method on the class side but still move the instance one there.'
]
