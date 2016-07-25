*Monticello-OldDataStreamCompatibility
readByteArray
	"PRIVATE -- Read the contents of a ByteArray."

	| count |
	count := byteStream nextNumber: 4.
	^ byteStream next: count  "assume stream is in binary mode"
