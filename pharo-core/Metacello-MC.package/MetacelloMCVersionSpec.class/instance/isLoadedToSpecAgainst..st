testing
isLoadedToSpecAgainst: resolvedPackageAndProjectNames
	"all loaded projects and packages match specifications (at least one package loaded)"

	(self isPartiallyCurrentAgainst: resolvedPackageAndProjectNames) isLoadedToSpec: [ :ignored | ^ true ].
	^ false