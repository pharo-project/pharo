"
I am a remove method strategy which do not remove methods but instead it shows all senders.
"
Class {
	#name : #ClyNotRemoveAndShowSendersMethodStrategy,
	#superclass : #ClyInteractiveRemoveMethodStrategy,
	#category : #'Calypso-SystemTools-QueryBrowser-Commands-Methods'
}

{ #category : #accessing }
ClyNotRemoveAndShowSendersMethodStrategy >> orderForBrowser [
	^3
]

{ #category : #execution }
ClyNotRemoveAndShowSendersMethodStrategy >> removeMethods: methods [
	
	(ClyShowMessageSenderCommand forMethods: methods by: browser) execute
]

{ #category : #execution }
ClyNotRemoveAndShowSendersMethodStrategy >> userRequestString [
	^'Don''t remove, but show me those senders'
]
