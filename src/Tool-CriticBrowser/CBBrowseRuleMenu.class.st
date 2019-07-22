"
ts
"
Class {
	#name : #CBBrowseRuleMenu,
	#superclass : #CmCommand,
	#category : #'Tool-CriticBrowser-utilities-contextMenu'
}

{ #category : #hooks }
CBBrowseRuleMenu >> execute [
	self context selectedRule ifNotNil: [ :aRule | aRule browse ]
]

{ #category : #initialization }
CBBrowseRuleMenu >> initialize [
	super initialize.
	self
		basicName: 'browse rule';
		basicDescription: 'Browse rule'
]
