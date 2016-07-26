*Monticello-OldDataStreamCompatibility
writeByteArray: aByteArray
	"PRIVATE -- Write the contents of a ByteArray."

	byteStream nextNumber: 4 put: aByteArray size.
	"May have to convert types here..."
	byteStream nextPutAll: aByteArray.