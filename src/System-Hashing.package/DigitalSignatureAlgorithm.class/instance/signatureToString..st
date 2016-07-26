public
signatureToString: aSignature
	"Answer a string representation of the given signature. This string can be parsed using the stringToSignature: method."

	| s |
	s := (String new: 2000) writeStream.
	s nextPutAll: '[DSA digital signature '.
	s nextPutAll: aSignature first printStringHex.
	s space.
	s nextPutAll: aSignature second printStringHex.
	s nextPutAll: ']'.
	^ s contents
