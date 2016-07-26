temp files
createTempFileToBeUsedAsReadStreamOn: aDirectoryPath
	"Important: for some reason, if we use MultiByteFileStream instances, we have some test failures.
	So for the moment, until we fix or understand this, always use StandardFileStream instances.
	
	This is a simple facility method if the user wants to use regular files rather than pipes.
	In the customization of defaultReadStreamCreationBlock: or defaultWriteStreamCreationBlock: 
	the user may want to use this method to automatically create a temp file on a aDirectoryPath.
	If you want another way of building temp files, then simply do not use this method and
	set the closure you want in defaultReadStreamCreationBlock: or defaultWriteStreamCreationBlock: 
	
	"
	
	^ StandardFileStream forceNewFileNamed: (aDirectoryPath asFileReference / self createTempFilename) fullName