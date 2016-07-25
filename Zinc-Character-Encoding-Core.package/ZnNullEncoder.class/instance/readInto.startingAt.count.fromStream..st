convenience
readInto: string startingAt: offset count: requestedCount fromStream: stream
	"Read requestedCount characters into string starting at offset,
	returning the number read, there could be less available when stream is atEnd"
	"Overwritten for performance reasons"

	| byteBuffer readCount |
	byteBuffer := ByteArray new: requestedCount.
	readCount := stream readInto: byteBuffer startingAt: 1 count: requestedCount.
	string replaceFrom: offset to: offset + readCount - 1 with: byteBuffer startingAt: 1.
	^ readCount