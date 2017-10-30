convenience
readInto: string startingAt: offset count: requestedCount fromStream: stream
	"Read requestedCount characters into string starting at offset,
	returning the number read, there could be less available when stream is atEnd"
	"Overwritten for performance reasons"

	offset to: offset + requestedCount - 1 do: [ :index |
		stream atEnd ifTrue: [ ^ index - offset ].  
		string byteAt: index put: stream next ].
	^ requestedCount