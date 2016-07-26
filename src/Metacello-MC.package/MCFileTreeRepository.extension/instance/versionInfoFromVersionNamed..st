*metacello-mc
versionInfoFromVersionNamed: aString
	^ self versionInfoForPackageDirectory: (self fileDirectoryOn: (aString, self packageExtension))