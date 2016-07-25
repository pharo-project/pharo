*Monticello-OldDataStreamCompatibility
readBitmap
	"PRIVATE -- Read the contents of a Bitmap."
	^ Bitmap newFromStream: byteStream
	"Note that the reader knows that the size is in long words, but the data is in bytes."