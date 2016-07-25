convenience
next: count putAll: string startingAt: offset toStream: stream
	"Write count characters from string starting at offset to stream."
	
	string isByteString
		ifTrue: [ self next: count putAllByteString: string startingAt: offset toStream: stream ]
		ifFalse: [ super next: count putAll: string startingAt: offset toStream: stream ]