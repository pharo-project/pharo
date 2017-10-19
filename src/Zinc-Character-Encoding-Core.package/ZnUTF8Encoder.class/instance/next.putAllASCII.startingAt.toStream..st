private
next: count putAllASCII: string startingAt: offset toStream: stream
	"Write count bytes from string starting at offset to stream,
	assuming all characters are in the ASCII set and need no translation"
	
	offset to: offset + count - 1 do: [ :index | 
		stream nextPut: (string byteAt: index) ]