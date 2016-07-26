halos and balloon help
boundsForBalloon
	"Some morphs have bounds that are way too big.  This is a contorted way of making things work okay in PluggableListMorphs, whose list elements historically have huge widths"

	| ownerOwner |
	^ ((owner notNil and: [(ownerOwner := owner owner) notNil]) and:
			[ownerOwner isKindOf: PluggableListMorph])
		ifTrue:
			[self boundsInWorld intersect: ownerOwner boundsInWorld ifNone: [self boundsInWorld] ]
		ifFalse:
			[super boundsForBalloon]