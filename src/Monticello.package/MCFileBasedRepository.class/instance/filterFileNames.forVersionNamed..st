accessing
filterFileNames: aCollection forVersionNamed: aString
	^ aCollection select: [:ea | (self versionNameFromFileName: ea) = aString] 