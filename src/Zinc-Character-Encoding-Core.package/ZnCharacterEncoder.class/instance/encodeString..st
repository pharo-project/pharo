convenience
encodeString: string
	"Encode string and return the resulting byte array"
	
	^ ByteArray streamContents: [ :stream |
		self next: string size putAll: string startingAt: 1 toStream: stream ]