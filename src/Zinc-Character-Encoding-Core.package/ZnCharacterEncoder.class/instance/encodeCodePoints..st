convenience
encodeCodePoints: codePoints
	"Encode codePoints and return the resulting byte array"
	
	^ ByteArray streamContents: [ :stream |
		codePoints do: [ :each | 
			self nextPutCodePoint: each toStream: stream ] ]