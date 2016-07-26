operations
accept: srcBuf from: start to: stop into: dstBuf
	"Start or continue the server handshake using the given input token"

	^ self primitiveSSL: handle accept: srcBuf startingAt: start count: stop - start + 1 into: dstBuf