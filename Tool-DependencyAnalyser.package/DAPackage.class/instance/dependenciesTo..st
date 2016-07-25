dependencies
dependenciesTo: aPackage
	^ dependencies select: [ :each | each target packageName = aPackage ].