protocol
putFileNamed: filePath as: fileNameOnServer
	"FTP a file to the server."


	| fileStream |
	fileStream := FileStream readOnlyFileNamed: filePath.
	fileStream
		ifNil: [(FileDoesNotExistException fileName: filePath) signal].
	self putFileStreamContents: fileStream as: fileNameOnServer
