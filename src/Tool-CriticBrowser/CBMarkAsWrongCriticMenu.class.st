Class {
	#name : #CBMarkAsWrongCriticMenu,
	#superclass : #CmCommand,
	#category : #'Tool-CriticBrowser-utilities-contextMenu'
}

{ #category : #hooks }
CBMarkAsWrongCriticMenu >> execute [
	"Execute the actions that should be done by the command.
	 This method expect that the context has been put in #context inst. var. if any context is relevant."

	| aCriticBrowser |
	aCriticBrowser := self context.
	aCriticBrowser isSelectedRuleIsFalsePositive
		ifTrue: [ aCriticBrowser
				removeFalsePositive: aCriticBrowser selectedCritic
				forRule: aCriticBrowser selectedRule ]
		ifFalse: [ aCriticBrowser
				addFalsePositive: aCriticBrowser selectedCritic
				forRule: aCriticBrowser selectedRule ].
	aCriticBrowser updateTree
]

{ #category : #initialization }
CBMarkAsWrongCriticMenu >> initialize [
	super initialize.
	self
		basicName: 'mark/unMark as wrong';
		basicDescription: 'mark/unMark as wrong'
]
