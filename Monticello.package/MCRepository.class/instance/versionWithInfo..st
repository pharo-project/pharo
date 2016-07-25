interface
versionWithInfo: aVersionInfo
	^ self versionWithInfo: aVersionInfo ifAbsent: [nil]