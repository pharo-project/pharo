encoding primitives
encode: aString

	"Should not return a string"
	^ ((ZnCharacterEncoder newForEncoding: 'utf8') encodeString: aString) asString