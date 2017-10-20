encoding
encodeEmbeddedSource
	"data is string with method's source code"

	| str length encodedLength utf8data |
	[ data isString ] assert.
	utf8data := data utf8Encoded.
	str := WriteStream on: (ByteArray new: utf8data size).
	length := utf8data size.
	encodedLength := self encodeLengthField: length.
	str nextPutAll: utf8data asByteArray.
	str nextPutAll: encodedLength.	"trailing byte"
	str nextPut: self kindAsByte + encodedLength size - 1.
	encodedData := str contents