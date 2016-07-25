private
next: anAmount putByteString: aString startingAt: startIndex toStream: aStream

	"This is a reasonable default which speeds up conversion of encodings which have a partial match with latin1 encoding (for instance ascii supersets)"
	| lastIndex nextIndex |
	lastIndex := startIndex.
	[nextIndex := ByteString findFirstInString: aString inSet: latin1Map startingAt: lastIndex.
	nextIndex = 0 or: [nextIndex >= (startIndex + anAmount) ]] whileFalse:
		[aStream next: nextIndex-lastIndex putAll: aString startingAt: lastIndex.
		aStream basicNextPutAll: (latin1Encodings at: (aString byteAt: nextIndex)+1).
		lastIndex := nextIndex + 1].
	aStream basicNext: anAmount - lastIndex + startIndex putAll: aString startingAt: lastIndex.