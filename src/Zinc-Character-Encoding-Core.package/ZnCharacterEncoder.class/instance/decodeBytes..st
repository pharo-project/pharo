convenience
decodeBytes: bytes
	"Decode bytes and return the resulting string"
	
	| byteStream |
	byteStream := bytes readStream.
	^ String streamContents: [ :stream |
		[ byteStream atEnd ] whileFalse: [
			stream nextPut: (self nextFromStream: byteStream) ] ] 