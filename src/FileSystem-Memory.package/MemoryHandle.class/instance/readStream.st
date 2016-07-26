stream-protocol
readStream
	"Return a readstream on my contents.
	Using myself as target collection allows to share the internal bytearray between multiple streams."
	^ entry readStream