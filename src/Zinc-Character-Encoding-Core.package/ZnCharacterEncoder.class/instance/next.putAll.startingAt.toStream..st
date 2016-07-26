convenience
next: count putAll: string startingAt: offset toStream: stream
	"Write count characters from string starting at offset to stream."
	
	0 to: count - 1 do: [ :each | 
		self nextPut: (string at: offset + each) toStream: stream ]