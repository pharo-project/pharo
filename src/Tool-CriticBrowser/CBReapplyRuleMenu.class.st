Class {
	#name : #CBReapplyRuleMenu,
	#superclass : #CmCommand,
	#category : #'Tool-CriticBrowser-utilities-contextMenu'
}

{ #category : #hooks }
CBReapplyRuleMenu >> execute [
	"Execute the actions that should be done by the command.
	 This method expect that the context has been put in #context inst. var. if any context is relevant."

	self context selectedRule
		ifNotNil: [ :aRule | 
			aRule isComposite
				ifTrue: [ aRule rules do: [ :each | self context reapplyRule: each ] ]
				ifFalse: [ self context reapplyRule: aRule ] ]
]

{ #category : #initialization }
CBReapplyRuleMenu >> initialize [
	super initialize.
	self
		basicName: 'Reapply this rule';
		basicDescription: 'Reapply this rule'
]
