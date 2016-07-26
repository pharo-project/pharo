operations
decrypt: srcBuf from: start to: stop into: dstBuf
	"Decrypt the input in srcBuf from start to stop into dstBuf."

	^ self primitiveSSL: handle decrypt: srcBuf startingAt: start count: stop - start + 1 into: dstBuf