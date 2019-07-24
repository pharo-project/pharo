"
I'm a command use to mark as 'To do' the selected critic 
"
Class {
	#name : #CBMarkAsTODOCriticCommand,
	#superclass : #CmCommand,
	#category : #'Tool-CriticBrowser-Commands'
}

{ #category : #hooks }
CBMarkAsTODOCriticCommand >> execute [
	| aCriticBrowser |
	aCriticBrowser := self context.
	aCriticBrowser isSelectedCriticToDo
		ifTrue: [ aCriticBrowser removeToDo: aCriticBrowser selectedCritic forRule: aCriticBrowser selectedRule ]
		ifFalse: [ aCriticBrowser addToDo: aCriticBrowser selectedCritic forRule: aCriticBrowser selectedRule ].
	aCriticBrowser updateTree
]

{ #category : #initialization }
CBMarkAsTODOCriticCommand >> initialize [
	super initialize.
	self basicName: 'Mark/unmark as TODO'
]
