loading
notCachedVersionWithInfo: aVersionInfo 
	(self allFileNamesForVersionNamed: aVersionInfo name)
		do: [ :fileName | 
			| version |
			version := self versionFromRepositoryFromFileNamed: fileName.
			version info = aVersionInfo
				ifTrue: [ ^ version ] ].
	^ nil