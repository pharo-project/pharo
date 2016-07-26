testing
isAllLoadedToSpecAgainst: resolvedPackageAndProjectNames
	"all projects and packages are loaded and match specification"

	(self isPartiallyCurrentAgainst: resolvedPackageAndProjectNames) isAllLoadedToSpec: [ :ignored | ^ true ].
	^ false