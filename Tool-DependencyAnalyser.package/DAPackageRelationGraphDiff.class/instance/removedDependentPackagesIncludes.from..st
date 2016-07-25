testing
removedDependentPackagesIncludes: aPackage from: parent
	^ self dependentPackagesDiffRemoved
		detect: [ :package | package content = aPackage and: [ package parent = parent ] ]
		ifFound: [ true ]
		ifNone: [ false ]