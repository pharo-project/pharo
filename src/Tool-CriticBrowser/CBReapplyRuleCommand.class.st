Class {
	#name : #CBReapplyRuleCommand,
	#superclass : #CBRuleCommand,
	#category : #'Tool-CriticBrowser-Commands'
}

{ #category : #hooks }
CBReapplyRuleCommand >> execute [
	self selectedRule isComposite ifTrue: [ self selectedRule rules do: [ :each | self context reapplyRule: each ] ] ifFalse: [ self context reapplyRule: self selectedRule ]
]

{ #category : #initialization }
CBReapplyRuleCommand >> initialize [
	super initialize.
	self
		basicName: 'Reapply';
		basicDescription: 'Reapply the selected rule.'
]
