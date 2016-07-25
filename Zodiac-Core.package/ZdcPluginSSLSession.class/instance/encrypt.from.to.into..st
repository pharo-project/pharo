operations
encrypt: srcBuf from: start to: stop into: dstBuf
	"Encrypt the input in srcBuf from start to stop into dstBuf."

	^ self primitiveSSL: handle encrypt: srcBuf startingAt: start count: stop - start + 1 into: dstBuf