accessing
latin1Encodings
	"Answer an Array mapping latin1 characters to conversion string"

	^latin1Encodings ifNil:
		[self initializeLatin1MapAndEncodings.
		latin1Encodings]