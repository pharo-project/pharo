convenience
readInto: string startingAt: offset count: requestedCount fromStream: stream
	"Read requestedCount characters into string starting at offset,
	returning the number read, there could be less available when stream is atEnd.
	The clean code is in my superclass"

	^ self optimizedReadInto: string startingAt: offset count: requestedCount fromStream: stream