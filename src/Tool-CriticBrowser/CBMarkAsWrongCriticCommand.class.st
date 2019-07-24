Class {
	#name : #CBMarkAsWrongCriticCommand,
	#superclass : #CmCommand,
	#category : #'Tool-CriticBrowser-Commands'
}

{ #category : #hooks }
CBMarkAsWrongCriticCommand >> execute [
	| aCriticBrowser |
	aCriticBrowser := self context.
	aCriticBrowser isSelectedRuleIsFalsePositive
		ifTrue: [ aCriticBrowser removeFalsePositive: aCriticBrowser selectedCritic forRule: aCriticBrowser selectedRule ]
		ifFalse: [ aCriticBrowser addFalsePositive: aCriticBrowser selectedCritic forRule: aCriticBrowser selectedRule ].
	aCriticBrowser updateTree
]

{ #category : #initialization }
CBMarkAsWrongCriticCommand >> initialize [
	super initialize.
	self basicName: 'Mark/unmark as wrong'
]
