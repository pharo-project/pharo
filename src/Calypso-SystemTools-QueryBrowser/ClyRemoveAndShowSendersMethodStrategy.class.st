"
I am a remove method strategy which removes methods but in addition it shows all senders.
"
Class {
	#name : #ClyRemoveAndShowSendersMethodStrategy,
	#superclass : #ClyInteractiveRemoveMethodStrategy,
	#category : #'Calypso-SystemTools-QueryBrowser-Commands-Methods'
}

{ #category : #accessing }
ClyRemoveAndShowSendersMethodStrategy >> orderForBrowser [
	^2
]

{ #category : #execution }
ClyRemoveAndShowSendersMethodStrategy >> removeMethods: methods [
	SycSilentlyRemoveMethodStrategy new removeMethods: methods.
	(ClyShowMessageSenderCommand forMethods: methods by: browser) execute
]

{ #category : #execution }
ClyRemoveAndShowSendersMethodStrategy >> userRequestString [
	^'Remove, then browse senders'
]
