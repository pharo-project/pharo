accessing
workingDirectoryPath: aPath

	aPath isAbsolute 
		ifFalse: [ self error: 'Cannot set the working directory to a relative path' ].
	workingDirectory := aPath
