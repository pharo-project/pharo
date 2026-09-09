Class {
	#name : 'RePushDownInstanceVariableTransformationChoice',
	#superclass : 'ReMethodChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
RePushDownInstanceVariableTransformationChoice >> action [

	driver pushDownInstanceVariable
]

{ #category : 'accessing' }
RePushDownInstanceVariableTransformationChoice >> description [

	^ 'Push down instance variable anyway'
]
