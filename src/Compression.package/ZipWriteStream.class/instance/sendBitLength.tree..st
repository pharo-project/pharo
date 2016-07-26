dynamic blocks
sendBitLength: bitLength tree: aTree
	"Send the given bitLength"
	encoder nextBits: (aTree bitLengthAt: bitLength) 
		put: (aTree codeAt: bitLength).