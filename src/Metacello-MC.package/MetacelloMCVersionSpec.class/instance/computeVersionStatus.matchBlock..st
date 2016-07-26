accessing
computeVersionStatus: resolvedPackageAndProjectNames matchBlock: matchBlock
	| status |
	status := resolvedPackageAndProjectNames isNil
		ifTrue: [ self isPartiallyCurrent ]
		ifFalse: [ self isPartiallyCurrentAgainst: resolvedPackageAndProjectNames ].
	status isAllLoadedToSpec: matchBlock.
	status isLoadedToSpec: matchBlock.
	status isLoadedMatchConstraints: matchBlock.
	status isSomethingLoaded: matchBlock