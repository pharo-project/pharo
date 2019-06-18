Extension { #name : #ClySuperclassVisibilityLevel }

{ #category : #'*Calypso-SystemPlugins-Traits-Queries' }
ClySuperclassVisibilityLevel >> asTraitVisibility [
	| traits |
	traits := extraClassScope basisObjects select: [ :each | each isTrait ].
	
	^(ClySingleTraitVisibilityLevel of: visibleClassScope)
		extraClassScope: (visibleClassScope asScope: ClyClassScope ofAll: traits)
]
