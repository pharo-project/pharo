convenience
next: count putAll: string startingAt: offset toStream: stream
	"Write count characters from string starting at offset to stream."
	"Overwritten for performance reasons - create a fast path for byte strings"
	
	string isByteString
		ifTrue: [ self next: count putAllByteString: string startingAt: offset toStream: stream ]
		ifFalse: [ super next: count putAll: string startingAt: offset toStream: stream ]