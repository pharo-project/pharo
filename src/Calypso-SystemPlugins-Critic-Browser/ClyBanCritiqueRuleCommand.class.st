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
ClyBanCritiqueRuleCommand >> applyResultInContext: aToolContext [
	super applyResultInContext: aToolContext.

	aToolContext tool criticView update
]

{ #category : 'table decoration' }
ClyBanCritiqueRuleCommand >> description [
	^'Ban this rule'
]

{ #category : 'execution' }
ClyBanCritiqueRuleCommand >> execute [

	critique guidedBan
]

{ #category : 'initialization' }
ClyBanCritiqueRuleCommand >> iconName [
	^#smallCancel
]
