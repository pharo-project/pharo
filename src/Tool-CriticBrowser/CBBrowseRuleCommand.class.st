"
ts
"
Class {
	#name : #CBBrowseRuleCommand,
	#superclass : #CBRuleCommand,
	#category : #'Tool-CriticBrowser-Commands'
}

{ #category : #hooks }
CBBrowseRuleCommand >> execute [
	self selectedRule browse
]

{ #category : #initialization }
CBBrowseRuleCommand >> initialize [
	super initialize.
	self
		basicName: 'Browse';
		basicDescription: 'Browse the class defining the rule.'
]
