Class {
	#name : 'RBRemoveAndShowSendersChoice',
	#superclass : 'RBRemoveMethodChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'execution' }
RBRemoveAndShowSendersChoice >> action [

	driver applyChanges.
	driver browseSenders
]

{ #category : 'accessing' }
RBRemoveAndShowSendersChoice >> description [

	^ 'Remove, then browse senders'
]
