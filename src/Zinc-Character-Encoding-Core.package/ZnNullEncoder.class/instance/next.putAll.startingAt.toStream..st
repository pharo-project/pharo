convenience
next: count putAll: string startingAt: offset toStream: stream
	"Write count bytes from string starting at offset to stream."
	"Overwritten for performance reasons"

	string isByteString
		ifTrue: [ 
			offset to: offset + count - 1 do: [ :index | 
				stream nextPut: (string byteAt: index) ] ]
		ifFalse: [ 
			super next: count putAll: string startingAt: offset toStream: stream ]