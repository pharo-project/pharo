file system
deleteFileNamed: filePath
	(self fileDirectoryClass on: filePath) containingDirectory deleteFileNamed: (self fileDirectoryClass localNameFor: filePath)