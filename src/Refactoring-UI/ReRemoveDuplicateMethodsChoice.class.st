Class {
	#name : 'ReRemoveDuplicateMethodsChoice',
	#superclass : 'ReRemoveMethodChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'execution' }
ReRemoveDuplicateMethodsChoice >> action [

	driver removeDuplicateMethods
]

{ #category : 'execution' }
ReRemoveDuplicateMethodsChoice >> description [

	^ 'Remove duplicate methods'
]
