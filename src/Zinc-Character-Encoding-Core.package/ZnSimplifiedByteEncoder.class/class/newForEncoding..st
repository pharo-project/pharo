instance creation
newForEncoding: string
	"Return a new character encoder object for an encoding described by string.
	We use our precomputed ByteTextConverters tables."

	| tables canonicalName |
	canonicalName := self canonicalEncodingIdentifier: string.
	tables := self byteTextConverters at: canonicalName.
	^ self new
		identifier: canonicalName;
		byteToUnicode: tables first;
		unicodeToByte: tables second;
		yourself