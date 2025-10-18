"
I am a command to show critique details.
I just show the dialog with critique description and title
"
Class {
	#name : 'ClyShowCritiqueDetailsCommand',
	#superclass : 'ClyCritiqueCommand',
	#category : 'Calypso-SystemPlugins-Critic-Browser',
	#package : 'Calypso-SystemPlugins-Critic-Browser'
}

{ #category : 'accessing' }
ClyShowCritiqueDetailsCommand >> defaultMenuIconName [
	^#smallQuestion
]

{ #category : 'table decoration' }
ClyShowCritiqueDetailsCommand >> description [
	^'View rationale behind the rule'
]

{ #category : 'execution' }
ClyShowCritiqueDetailsCommand >> execute [

	self morphicUIManager longMessage: critique description title: critique title
]
