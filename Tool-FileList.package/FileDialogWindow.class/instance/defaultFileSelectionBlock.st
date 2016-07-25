accessing
defaultFileSelectionBlock
	"Answer the default file selection block."

	^[:entry |
		entry isDirectory
			ifTrue: [self showDirectoriesInFileList]
			ifFalse: [self fileNamePattern match: entry basename]]