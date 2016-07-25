convenience
encodeStringWithByteOrderMark: string
	"Encode string and return the resulting byte array.
	Always add a Unicode byte order mark (BOM) in front."
	
	^ ByteArray streamContents: [ :stream |
		self nextPutByteOrderMarkToStream: stream.
		self next: string size putAll: string startingAt: 1 toStream: stream ]