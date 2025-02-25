Class {
	#name : 'ReDontRemoveButShowSendersChoice',
	#superclass : 'ReRemoveMethodChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'execution' }
ReDontRemoveButShowSendersChoice >> action [

	driver browseSenders
]

{ #category : 'accessing' }
ReDontRemoveButShowSendersChoice >> description [

	^ 'Don''t remove, but show me those senders'
]
