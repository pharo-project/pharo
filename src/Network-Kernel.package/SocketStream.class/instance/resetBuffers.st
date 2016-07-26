private
resetBuffers
	"Recreate the buffers with default start sizes."

	inBuffer := self streamBuffer: bufferSize.
	lastRead := 0.
	inNextToWrite := 1.
	outBuffer := self streamBuffer: bufferSize.
	outNextToWrite := 1