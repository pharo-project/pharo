instance creation
decompressFromByteArray: byteArray 
	| s bitmap size |
	s := byteArray readStream.
	size := self decodeIntFrom: s.
	bitmap := self new: size.
	bitmap 
		decompress: bitmap
		fromByteArray: byteArray
		at: s position + 1.
	^ bitmap