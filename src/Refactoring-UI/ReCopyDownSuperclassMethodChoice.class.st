Class {
	#name : 'ReCopyDownSuperclassMethodChoice',
	#superclass : 'ReMethodChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
ReCopyDownSuperclassMethodChoice >> action [

	driver copyDownMethods
]

{ #category : 'accessing' }
ReCopyDownSuperclassMethodChoice >> description [

	^ 'Copy down the superclass method to the classes that dont define it'
]
