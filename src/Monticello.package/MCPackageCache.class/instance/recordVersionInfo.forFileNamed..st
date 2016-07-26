accessing
recordVersionInfo: aVersionInfo forFileNamed: aString
	fileNames at: aVersionInfo put: aString.
	sorter addVersionInfo: aVersionInfo