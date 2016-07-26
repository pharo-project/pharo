accessing
latin1Map
	"Answer a ByteArray map telling if latin1 characters needs conversion or not"

	^latin1Map ifNil:
		[self initializeLatin1MapAndEncodings.
		latin1Map]