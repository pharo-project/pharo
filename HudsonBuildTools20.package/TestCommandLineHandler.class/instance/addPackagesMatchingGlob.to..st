accessing
addPackagesMatchingGlob: aGlobString to: aSet
	RPackage organizer packageNames do: [ :packageName|
		(aGlobString match: packageName)
			ifTrue: [ aSet add: packageName ]]