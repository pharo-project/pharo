"
Pay attention this choice is for performing a transformation when the user wants to overpass the blocking preconditions.
"
Class {
	#name : 'RePullUpMethodTransformationChoice',
	#superclass : 'ReMethodChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
RePullUpMethodTransformationChoice >> action [

	driver pullUpMethods
]

{ #category : 'accessing' }
RePullUpMethodTransformationChoice >> description [

	^ 'Pull up methods anyway'
]
