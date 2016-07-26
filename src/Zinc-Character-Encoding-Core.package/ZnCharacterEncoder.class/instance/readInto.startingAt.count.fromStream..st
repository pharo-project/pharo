convenience
readInto: string startingAt: offset count: requestedCount fromStream: stream
	"Read requestedCount characters into string starting at offset,
	returning the number read, there could be less available when stream is atEnd"

	0 to: requestedCount - 1 do: [ :count |
		stream atEnd ifTrue: [ ^ count ].  
		string at: offset + count put: (self nextFromStream: stream) ].
	^ requestedCount