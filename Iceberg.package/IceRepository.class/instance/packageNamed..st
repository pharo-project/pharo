querying
packageNamed: packageName
	^ (savedPackages ifNil: [ self loadSavedPackages ]) at: packageName
