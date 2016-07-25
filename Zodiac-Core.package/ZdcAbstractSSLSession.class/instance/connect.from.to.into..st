operations
connect: srcBuf from: start to: stop into: dstBuf
	"Start or continue the client handshake using the given input token"

	self subclassResponsibility