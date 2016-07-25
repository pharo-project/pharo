querying
savedPackages
	^ (savedPackages ifNil: [ self loadSavedPackages ]) values
