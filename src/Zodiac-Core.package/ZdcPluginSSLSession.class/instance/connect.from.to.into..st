operations
connect: srcBuf from: start to: stop into: dstBuf
	"Start or continue the client handshake using the given input token"

	^ self primitiveSSL: handle connect: srcBuf startingAt: start count: stop - start + 1 into: dstBuf