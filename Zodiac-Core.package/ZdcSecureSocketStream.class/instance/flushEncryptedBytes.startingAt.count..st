private out
flushEncryptedBytes: bytes startingAt: offset count: count
	"Write encrypted bytes to the network."
	
	^ super flushBytes: bytes startingAt: offset count: count