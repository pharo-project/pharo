interface
versionWithInfo: aVersionInfo ifAbsent: errorBlock
	(self allFileNamesForVersionNamed: aVersionInfo name)
		ifNotEmpty: [ :aCollection | ^ self versionFromFileNamed: aCollection first ].
	^ errorBlock value