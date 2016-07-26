converting
decode: string
	"Decode a percent encoded string and return the resulting byte array.
	After percent decoding I will use my character encoder to convert byte values to characters."

	| bytes stringStream |
	stringStream := string readStream.
	bytes := ByteArray streamContents: [ :byteStream | 
		self decode: stringStream to: byteStream ].
	^ self characterEncoder decodeBytes: bytes