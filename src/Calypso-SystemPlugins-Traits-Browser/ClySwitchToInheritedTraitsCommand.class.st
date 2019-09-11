"
I a command to show in browser a hierarchy of traits which are inherited by selected classes
"
Class {
	#name : #ClySwitchToInheritedTraitsCommand,
	#superclass : #ClySwitchTraitHierarchyModeCommand,
	#category : #'Calypso-SystemPlugins-Traits-Browser'
}

{ #category : #testing }
ClySwitchToInheritedTraitsCommand class >> canBeExecutedInContext: aToolContext [
	(super canBeExecutedInContext: aToolContext) ifFalse: [ ^false ].
	
	^aToolContext isSelectedClassUsesTraits
]

{ #category : #accessing }
ClySwitchToInheritedTraitsCommand >> defaultMenuItemName [
	^'Traits'
]

{ #category : #execution }
ClySwitchToInheritedTraitsCommand >> requiredQueryResult [
	^ClyTraitUserHierarchy inverse asQueryResult 
]

{ #category : #execution }
ClySwitchToInheritedTraitsCommand >> traitScopeClass [
	^ClyInheritedTraitScope 
]
