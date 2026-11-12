"
Ban this rule
"
Class {
	#name : 'ClyBanCritiqueRuleCommand',
	#superclass : 'ClyCritiqueCommand',
	#category : 'Calypso-SystemPlugins-Critic-Browser',
	#package : 'Calypso-SystemPlugins-Critic-Browser'
}

{ #category : 'execution' }
ClyBanCritiqueRuleCommand >> applyCommandResult [
	super applyCommandResult.

	self context tool criticView update
]

{ #category : 'execution' }
ClyBanCritiqueRuleCommand >> executeRefactoring [

	self critique guidedBan
]

{ #category : 'initialization' }
ClyBanCritiqueRuleCommand >> iconName [
	^#smallCancel
]
