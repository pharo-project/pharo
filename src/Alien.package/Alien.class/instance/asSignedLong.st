converting
asSignedLong "^<Integer>"
	"Answer an Integer created from the first 4 bytes of the receiver interpreted as a signed value."
	
	^self signedLongAt: 1