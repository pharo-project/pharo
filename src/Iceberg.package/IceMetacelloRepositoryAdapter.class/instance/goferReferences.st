accessing
goferReferences
	^ self repository savedPackages collect: [ :package | 
		GoferResolvedReference name: package latestVersion info name repository: self ]