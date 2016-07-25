accessing-files
deleteFileNamed: aFilename
	((Smalltalk at: #FileDirectory) forFileName: aFilename) deleteFileNamed: aFilename