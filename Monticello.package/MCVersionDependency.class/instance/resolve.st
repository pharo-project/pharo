resolving
resolve
	^ self repositoryGroup
		versionWithInfo: versionInfo
		ifNone: [ MCRepositoryGroup default versionWithInfo: versionInfo ifNone: []]