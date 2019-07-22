Class {
	#name : #CBMarkAsTODOCriticMenu,
	#superclass : #CmCommand,
	#category : #'Tool-CriticBrowser-utilities-contextMenu'
}

{ #category : #hooks }
CBMarkAsTODOCriticMenu >> execute [
	"Execute the actions that should be done by the command.
	 This method expect that the context has been put in #context inst. var. if any context is relevant."

	| aCriticBrowser |
	aCriticBrowser := self context.
	aCriticBrowser isSelectedCriticIsToDo
		ifTrue: [ aCriticBrowser
				removeToDo: aCriticBrowser selectedCritic
				forRule: aCriticBrowser selectedRule ]
		ifFalse: [ aCriticBrowser
				addToDo: aCriticBrowser selectedCritic
				forRule: aCriticBrowser selectedRule ].
	aCriticBrowser updateTree
]

{ #category : #initialization }
CBMarkAsTODOCriticMenu >> initialize [
	super initialize.
	self
		basicName: 'mark/unMark as TODO';
		basicDescription: 'mark/unMark as TODO'
]
