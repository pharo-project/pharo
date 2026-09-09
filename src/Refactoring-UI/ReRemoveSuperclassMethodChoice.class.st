Class {
	#name : 'ReRemoveSuperclassMethodChoice',
	#superclass : 'ReMethodChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
ReRemoveSuperclassMethodChoice >> action [ 

driver removeSuperclassMethod
]

{ #category : 'accessing' }
ReRemoveSuperclassMethodChoice >> description [

	^ 'Remove the superclass method'
]
