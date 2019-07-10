Extension { #name : #ClySwitchToFullHierarchyCommand }

{ #category : #'*Calypso-SystemPlugins-Traits-Browser' }
ClySwitchToFullHierarchyCommand class >> canBeExecutedInContext: aToolContext [
	(super canBeExecutedInContext: aToolContext) ifFalse: [ ^false ].
	
	^aToolContext isTraitSelected not
]
