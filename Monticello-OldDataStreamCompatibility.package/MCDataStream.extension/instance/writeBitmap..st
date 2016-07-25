*Monticello-OldDataStreamCompatibility
writeBitmap: aBitmap
	"PRIVATE -- Write the contents of a Bitmap."

	aBitmap writeOn: byteStream
	"Note that this calls (byteStream nextPutAll: aBitmap) which knows enough to put 4-byte quantities on the stream!  Reader must know that size is in long words."