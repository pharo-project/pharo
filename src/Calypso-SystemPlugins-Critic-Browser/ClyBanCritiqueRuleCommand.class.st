"
I am a command to ban given critique rule
"
Class {
	#name : #ClyBanCritiqueRuleCommand,
	#superclass : #ClyCritiqueCommand,
	#category : #'Calypso-SystemPlugins-Critic-Browser'
}

{ #category : #execution }
ClyBanCritiqueRuleCommand >> applyResultInContext: aToolContext [
	super applyResultInContext: aToolContext.

	aToolContext tool criticView update
]

{ #category : #initialization }
ClyBanCritiqueRuleCommand >> defaultMenuIconName [
	^#smallCancelIcon
]

{ #category : #'table decoration' }
ClyBanCritiqueRuleCommand >> description [
	^'Ban this rule'
]

{ #category : #execution }
ClyBanCritiqueRuleCommand >> execute [

	critique guidedBan 
]
