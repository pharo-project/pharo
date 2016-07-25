testing
isSomethingLoadedAgainst: resolvedPackageAndProjectNames
	"at least one package loaded"

	(self isPartiallyCurrentAgainst: resolvedPackageAndProjectNames) isSomethingLoaded: [ :ignored | ^ true ].
	^ false