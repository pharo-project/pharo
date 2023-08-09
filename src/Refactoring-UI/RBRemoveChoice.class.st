Class {
	#name : #RBRemoveChoice,
	#superclass : #RBRemoveMethodChoice,
	#category : #'Refactoring-UI-Choices'
}

{ #category : #accessing }
RBRemoveChoice >> action [

	refactoring removeMethodChanges
]

{ #category : #accessing }
RBRemoveChoice >> description [

	^ 'Remove it'
]
