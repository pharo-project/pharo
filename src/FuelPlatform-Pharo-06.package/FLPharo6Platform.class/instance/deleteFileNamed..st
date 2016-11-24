accessing-files
deleteFileNamed: aFilename
	((Smalltalk at: #File) named: aFilename) delete