stream-protocol
writeStream
	"Return a writestream on my contents.
	Using myself as target collection allows to share the internal bytearray between multiple streams."
	^ WriteStream on: self from: 1 to: entry fileSize