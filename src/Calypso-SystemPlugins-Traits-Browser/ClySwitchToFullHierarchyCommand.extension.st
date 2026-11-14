Extension { #name : 'ClySwitchToFullHierarchyCommand' }

{ #category : '*Calypso-SystemPlugins-Traits-Browser' }
ClySwitchToFullHierarchyCommand >> canBeExecuted [

	super canBeExecuted ifFalse: [ ^ false ].

	^ self context isTraitSelected not
]
