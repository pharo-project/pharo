operations
accept: srcBuf from: start to: stop into: dstBuf
	"Start or continue the server handshake using the given input token"

	self subclassResponsibility