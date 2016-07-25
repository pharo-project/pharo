accessing
allFilesWithExtension: anExtension
	^ self arguments select: [ :arg|
		arg endsWith: anExtension ]