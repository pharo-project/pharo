file system
readStreamOn: aFileHandle do: aBlock
	"Get a read stream on the file handle and execute some actions on it."
	^ aFileHandle readStreamDo: aBlock