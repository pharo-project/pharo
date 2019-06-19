"
I a command to show in browser a hierarchy of traits which are used by selected classes
"
Class {
	#name : #ClySwitchToTraitUsersCommand,
	#superclass : #ClySwitchTraitHierarchyModeCommand,
	#category : #'Calypso-SystemPlugins-Traits-Browser'
}

{ #category : #testing }
ClySwitchToTraitUsersCommand class >> canBeExecutedInContext: aToolContext [
	(super canBeExecutedInContext: aToolContext) ifFalse: [ ^false ].
	
	^aToolContext isTraitSelected
]

{ #category : #accessing }
ClySwitchToTraitUsersCommand >> defaultMenuItemName [
	^'Users'
]

{ #category : #execution }
ClySwitchToTraitUsersCommand >> requiredQueryResult [
	^ClyTraitUserHierarchy new asQueryResult 
]

{ #category : #execution }
ClySwitchToTraitUsersCommand >> traitScopeClass [
	^ClyTraitUserScope 
]
