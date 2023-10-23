Class {
	#name : 'RBDontRemoveButShowSendersChoice',
	#superclass : 'RBRemoveMethodChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
RBDontRemoveButShowSendersChoice >> action [

	driver browseSenders
]

{ #category : 'accessing' }
RBDontRemoveButShowSendersChoice >> description [

	^ 'Don''t remove, but show me those senders'
]
