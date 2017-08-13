accessing
addPackagesMatchingRegex: aRegex to: aSet
	RPackage organizer packageNames do: [ :packageName|
		(aRegex matches: packageName)
			ifTrue: [ aSet add: packageName ]]