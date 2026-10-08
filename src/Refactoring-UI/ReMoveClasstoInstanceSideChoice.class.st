Class {
	#name : 'ReMoveClasstoInstanceSideChoice',
	#superclass : 'ReMethodChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
ReMoveClasstoInstanceSideChoice >> action [

	driver applyChanges
]

{ #category : 'accessing' }
ReMoveClasstoInstanceSideChoice >> description [

	^ 'There is a method on the instance side but still move the class one there.'
]
