configuration
inBufferSize
	"Answers the current size of data in the inBuffer."

	^inNextToWrite - lastRead - 1