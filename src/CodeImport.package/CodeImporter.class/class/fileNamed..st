instance creation
fileNamed: aFileName
	^ self new file: (FileStream readOnlyFileNamed: aFileName).
