Class {
	#name : #CBImplementorCriticMenu,
	#superclass : #CmCommand,
	#category : #'Tool-CriticBrowser-utilities-contextMenu'
}

{ #category : #hooks }
CBImplementorCriticMenu >> execute [
	"Execute the actions that should be done by the command.
	 This method expect that the context has been put in #context inst. var. if any context is relevant."

	self context selectedCritic
		ifNotNil: [ :aCritic | 
			(aCritic sourceAnchor entity respondsTo: #selector)
				ifFalse: [ ^ self inform: 'you can ask the implementors of this' ].
			ClyQueryBrowser
				browseImplementorsOf: aCritic sourceAnchor entity selector ]
]

{ #category : #initialization }
CBImplementorCriticMenu >> initialize [
	super initialize.
	self
		basicName: 'implementor';
		basicDescription: 'implementor of selectedCritic'
]
