accessing
repositoryName
	^ self class repositoryURLToNameMappings 
		at: self repositoryUrl
		ifAbsent: [ self class unknownRepository ]