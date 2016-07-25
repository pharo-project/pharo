convenience
decodeAsCodePoints: bytes
	"Decode bytes and return the resulting code points"
	
	| byteStream |
	byteStream := bytes readStream.
	^ Array streamContents: [ :stream |
		[ byteStream atEnd ] whileFalse: [
			stream nextPut: (self nextCodePointFromStream: byteStream) ] ] 