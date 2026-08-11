Extension { #name : 'ClySuperclassVisibilityLevel' }

{ #category : '*Calypso-SystemPlugins-Traits-Queries' }
ClySuperclassVisibilityLevel >> asTraitVisibility [

	| traits |
	traits := self extraClassScope basisObjects select: [ :each | each isTrait ].

	^ (ClySingleTraitVisibilityLevel of: self visibleClassScope) extraClassScope:
		  (self visibleClassScope asScope: ClyClassScope ofAll: traits)
]
