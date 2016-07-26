*Compression-Streams
encodeUsingZip

	"data is string, encode it using gzip compression"
	| utf8str stream length encodedLength |

	[data isString] assert.
	utf8str := data convertToEncoding: 'utf8'.
	
	stream := ((ZipWriteStream on: (ByteArray new: utf8str size))
		nextPutAll: utf8str asByteArray;
		close;
		encodedStream).
		
	length := stream position.
	encodedLength := self encodeLengthField: length.

	stream nextPutAll: encodedLength.
	"trailing byte"
	stream nextPut: (self kindAsByte + encodedLength size - 1).
	
	encodedData := stream contents
	