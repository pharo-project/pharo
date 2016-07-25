interface
versionWithInfo: aVersionInfo ifAbsent: errorBlock
	(self allFileNamesForVersionNamed: aVersionInfo name) do: [:fileName | | version |
			version := self versionFromFileNamed: fileName.
			version info = aVersionInfo ifTrue: [^ version]].
	^ errorBlock value