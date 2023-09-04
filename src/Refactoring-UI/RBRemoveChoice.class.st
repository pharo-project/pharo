Class {
	#name : #RBRemoveChoice,
	#superclass : #RBRemoveMethodChoice,
	#category : #'Refactoring-UI-Choices'
}

{ #category : #accessing }
RBRemoveChoice >> action [

	driver removeMethodChanges
]

{ #category : #accessing }
RBRemoveChoice >> description [

	^ 'Remove it'
]
