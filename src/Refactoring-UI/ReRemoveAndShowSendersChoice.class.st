Class {
	#name : 'ReRemoveAndShowSendersChoice',
	#superclass : 'ReRemoveMethodChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'execution' }
ReRemoveAndShowSendersChoice >> action [

	driver applyChanges.
	driver browseSenders
]

{ #category : 'accessing' }
ReRemoveAndShowSendersChoice >> description [

	^ 'Remove, then browse senders'
]
