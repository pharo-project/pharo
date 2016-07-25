testing
isLoadedMatchConstraintsAgainst: resolvedPackageAndProjectNames
	"all loaded projects and packages match constraints (at least one package loaded)"

	(self isPartiallyCurrentAgainst: resolvedPackageAndProjectNames) isLoadedMatchConstraints: [ :ignored | ^ true ].
	^ false