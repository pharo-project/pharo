writting
writeByteArray: byteArray
	self writeInt32: byteArray size.
	stream nextPutAll: byteArray.