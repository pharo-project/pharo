writting
writeByteArray: byteArray
	self writeInt: byteArray size.
	stream nextPutAll: byteArray.