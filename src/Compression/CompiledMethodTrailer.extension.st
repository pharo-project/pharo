Extension { #name : #CompiledMethodTrailer }

{ #category : #'*Compression-Streams' }
CompiledMethodTrailer >> decodeEmbeddedSourceZip [

	"data is string with method's source code, compressed using zip compression"
	self decodeZip.
]

{ #category : #'*Compression-Streams' }
CompiledMethodTrailer >> decodeZip [
	"data := <trailer> unzip utf8ToSqueak"
	
	| len bytes |
	len := self decodeLengthField.
	bytes := ByteArray new: len.
	1 to: len do: [ :i |
		bytes at: i put: (method at: method size - size + i) ].
	data := (ZipReadStream on: bytes) contents asString convertFromEncoding: 'utf8'
]

{ #category : #'*Compression-Streams' }
CompiledMethodTrailer >> encodeEmbeddedSourceZip [

	"data is string with method's source code, encode it using Zip compression method"
	self encodeUsingZip

]

{ #category : #'*Compression-Streams' }
CompiledMethodTrailer >> encodeUsingZip [

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
	
]
