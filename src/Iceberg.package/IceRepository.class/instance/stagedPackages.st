accessing
stagedPackages
	^ self stagedPackageNames
		collect: [ :packageName | self packageNamed: packageName ]