converting
asSignedByte "^<Integer>"
	"Answer an Integer created from the first byte of the receiver interpreted as a signed value."
	
	^self signedByteAt: 1