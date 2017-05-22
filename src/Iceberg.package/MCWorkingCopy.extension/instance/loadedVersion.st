*Iceberg-Adapters
loadedVersion
	self ancestors do: [ :ancestor |
		(self repositoryGroup 
			versionWithInfo: ancestor
			ifNone: [ MCRepositoryGroup default versionWithInfo: ancestor ifNone: [] ])
			ifNotNil: [ :version | ^ version ]
	].
