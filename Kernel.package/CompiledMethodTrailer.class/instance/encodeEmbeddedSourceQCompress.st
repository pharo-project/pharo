encoding
encodeEmbeddedSourceQCompress
	"data is string with method's source code, encode it using qCompress method"

	| str length encodedLength |
	[ data isString ] assert.
	str := self qCompress: data.
	length := str position.
	encodedLength := self encodeLengthField: length.
	str nextPutAll: encodedLength.	"trailing byte"
	str nextPut: self kindAsByte + encodedLength size - 1.
	encodedData := str contents