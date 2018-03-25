accessing
repositoryLocation
	^ (Smalltalk os environment
		at: 'BOOTSTRAP_REPOSITORY'
		ifAbsent: [ '.' ]) asFileReference