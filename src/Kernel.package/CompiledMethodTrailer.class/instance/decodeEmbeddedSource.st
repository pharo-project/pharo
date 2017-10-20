decoding
decodeEmbeddedSource

	"data is string with method's source code"
	| len input dataStr |
	len := self decodeLengthField.
 	input := (ReadStream on: method asByteArray from: method size - size+1 to: method size - size + len).
	dataStr := WriteStream on: (ByteArray new: len).
	[ input atEnd ] whileFalse: [ dataStr nextPut: input next ].
	data := dataStr contents utf8Decoded