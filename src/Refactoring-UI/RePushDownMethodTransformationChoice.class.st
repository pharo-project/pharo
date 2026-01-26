"
Pay attention this choice is for performing a transformation when the user wants to overpass the blocking preconditions.
"
Class {
	#name : 'RePushDownMethodTransformationChoice',
	#superclass : 'ReMethodChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
RePushDownMethodTransformationChoice >> action [

	driver pushDownMethods
]

{ #category : 'accessing' }
RePushDownMethodTransformationChoice >> description [

	^ 'Push down methods anyway'
]
