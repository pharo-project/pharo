converting
asSignedShort "^<Integer>"
	"Answer an Integer created from the first 2 bytes of the receiver interpreted as a signed value."
	
	^self signedShortAt: 1