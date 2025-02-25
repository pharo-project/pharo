Class {
	#name : 'RePushDownMethodChoice',
	#superclass : 'ReMethodChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
RePushDownMethodChoice >> action [

	driver pushDownMethods
]

{ #category : 'accessing' }
RePushDownMethodChoice >> description [

	^ 'Push down methods anyway'
]
