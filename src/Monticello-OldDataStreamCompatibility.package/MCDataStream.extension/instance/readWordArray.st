*Monticello-OldDataStreamCompatibility
readWordArray
	"PRIVATE -- Read the contents of a WordArray."
	^ WordArray newFromStream: byteStream
	"Size is number of long words."