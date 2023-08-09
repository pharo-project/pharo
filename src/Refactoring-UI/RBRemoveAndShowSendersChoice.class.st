Class {
	#name : #RBRemoveAndShowSendersChoice,
	#superclass : #RBRemoveMethodChoice,
	#category : #'Refactoring-UI-Choices'
}

{ #category : #accessing }
RBRemoveAndShowSendersChoice >> action [ 
	
	driver removeMethodChanges.
	driver browseSenders.
]

{ #category : #accessing }
RBRemoveAndShowSendersChoice >> description [

	^ 'Remove, then browse senders'
]
