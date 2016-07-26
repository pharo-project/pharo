interface
versionWithInfo: aVersionInfo ifAbsent: errorBlock
	^ dict at: aVersionInfo ifAbsent: errorBlock