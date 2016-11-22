*UnifiedFFI
readStringUTF8
	"Assume that the receiver represents a C string containing UTF8 characters and convert it to a Smalltalk string."

	| stream index char |
	type isPointerType ifFalse: [self error: 'External object is not a pointer type.'].
	stream := WriteStream on: ByteArray new.
	index := 1.
	[(char := handle unsignedByteAt: index) = 0 ] whileFalse: [
		stream nextPut: char.
		index := index + 1].
	^ ZnCharacterEncoder utf8 decodeBytes: stream contents