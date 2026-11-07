"
I am a command to ban given critique rule
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

{ #category : 'table decoration' }
ClyBanCritiqueRuleCommand >> description [
	^'Ban this rule'
]

{ #category : 'execution' }
ClyBanCritiqueRuleCommand >> executeRefactoring [

	self critique guidedBan
]

{ #category : 'initialization' }
ClyBanCritiqueRuleCommand >> iconName [
	^#smallCancel
]
