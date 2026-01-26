Class {
	#name : 'ReMoveClassSideChoice',
	#superclass : 'ReMethodChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
ReMoveClassSideChoice >> action [

	driver applyChanges
]

{ #category : 'accessing' }
ReMoveClassSideChoice >> description [

	^ 'There is a method on the class side but still move the instance one there.'
]
