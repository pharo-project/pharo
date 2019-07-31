"
I'm a command use to reapply all rule (it's reset the cache and reload all)
"
Class {
	#name : #CBReapplyRuleCommand,
	#superclass : #CBRuleCommand,
	#category : #'Tool-CriticBrowser-Commands'
}

{ #category : #hooks }
CBReapplyRuleCommand >> execute [
	self selectedRule CBCommandExecuteReapplyRuleOn: self.
]

{ #category : #initialization }
CBReapplyRuleCommand >> initialize [
	super initialize.
	self
		basicName: 'Reapply';
		basicDescription: 'Reapply the selected rule.'
]
