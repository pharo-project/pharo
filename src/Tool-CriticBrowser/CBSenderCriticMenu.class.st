"
test
"
Class {
	#name : #CBSenderCriticMenu,
	#superclass : #CmCommand,
	#category : #'Tool-CriticBrowser-utilities-contextMenu'
}

{ #category : #hooks }
CBSenderCriticMenu >> execute [
	"Execute the actions that should be done by the command.
	 This method expect that the context has been put in #context inst. var. if any context is relevant."

	self context selectedCritic
		ifNotNil: [ :aCritic | 
			(aCritic sourceAnchor entity respondsTo: #selector)
				ifFalse: [ ^ self inform: 'you can`t use senders of this' ].
			ClyQueryBrowser
				browseSendersOf: aCritic sourceAnchor entity selector ]
]

{ #category : #hooks }
CBSenderCriticMenu >> initialize [
	super initialize.
	self
		basicName: 'senders';
		basicDescription: 'show the senders of selectedItem'
]
