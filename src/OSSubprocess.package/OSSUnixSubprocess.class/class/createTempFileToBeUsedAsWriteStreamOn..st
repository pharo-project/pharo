temp files
createTempFileToBeUsedAsWriteStreamOn: aDirectoryPath
	"See comment of #createTempFileToBeUsedAsReadStreamOn:."
	^  StandardFileStream forceNewFileNamed: (aDirectoryPath asFileReference / self createTempFilename) fullName