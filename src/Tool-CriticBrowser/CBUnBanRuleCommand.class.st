"
I'm a command use to un ban a rule (it's like un mark asFalsePositive all his critics)
"
Class {
	#name : #CBUnBanRuleCommand,
	#superclass : #CBRuleCommand,
	#category : #'Tool-CriticBrowser-Commands'
}

{ #category : #hooks }
CBUnBanRuleCommand >> execute [
	self selectedRule CBCommandExecuteUnBanRule: self
]

{ #category : #initialization }
CBUnBanRuleCommand >> initialize [
	super initialize.
	self
		basicName: 'Unban from selected packages';
		basicDescription: 'Unban from selected packages it`s like unmark as a false positive.'
]
