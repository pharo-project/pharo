"
I am a context menu group which holds all generated ClySwitchQueryScopeCommand items
"
Class {
	#name : #ClySwitchScopeMenuGroup,
	#superclass : #CmdMenuGroup,
	#category : #'Calypso-SystemTools-QueryBrowser-Commands-Controlling'
}

{ #category : #parents }
ClySwitchScopeMenuGroup class >> parentGroup [
	^ClyQueryMenuGroup 
]

{ #category : #accessing }
ClySwitchScopeMenuGroup >> name [
	^'Switch scope to'
]

{ #category : #accessing }
ClySwitchScopeMenuGroup >> order [
	^50000
]
