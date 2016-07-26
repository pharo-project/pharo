*Compression-Streams
decodeZip
	"data := <trailer> unzip utf8ToSqueak"
	
	| len bytes |
	len := self decodeLengthField.
	bytes := ByteArray new: len.
	1 to: len do: [ :i |
		bytes at: i put: (method at: method size - size + i) ].
	data := (ZipReadStream on: bytes) contents asString convertFromEncoding: 'utf8'