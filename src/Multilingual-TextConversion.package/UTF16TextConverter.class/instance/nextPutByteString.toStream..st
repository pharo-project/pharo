private
nextPutByteString: aString toStream: aStream
	| utf16Bytes start |
	"No bytestring character will be a surrogate pair, so simply write the 2x size string with the correct endianness"
	utf16Bytes := String new: aString size* 2.
	start := useLittleEndian ifTrue: [1] ifFalse: [2].
	start to: utf16Bytes size by: 2 do: [:ix | utf16Bytes at: ix put: (aString at: ix +1 // 2 )].
	aStream basicNextPutAll: utf16Bytes.
	