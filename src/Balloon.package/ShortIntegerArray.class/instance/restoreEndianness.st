objects from disk
restoreEndianness
	"This word object was just read in from a stream.  It was stored in Big Endian (Mac) format.  Swap each pair of bytes (16-bit word), if the current machine is Little Endian.
	Why is this the right thing to do?  We are using memory as a byteStream.  High and low bytes are reversed in each 16-bit word, but the stream of words ascends through memory.  Different from a Bitmap."

	EndianDetector isLittleEndian
		ifTrue: [ self swapBytePairsHacky ]