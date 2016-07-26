*Monticello-OldDataStreamCompatibility
writeWordLike: aWordArray
	"Note that we put the class name before the size."

	self nextPut: aWordArray class name.
	aWordArray writeOn: byteStream
	"Note that this calls (byteStream nextPutAll: aBitmap) which knows enough to put 4-byte quantities on the stream!  Reader must know that size is in long words or double-bytes."