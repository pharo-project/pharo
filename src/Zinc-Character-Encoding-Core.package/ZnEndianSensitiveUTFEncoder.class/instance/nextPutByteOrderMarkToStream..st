convenience
nextPutByteOrderMarkToStream: stream
	self write16BitWord: self byteOrderMark toStream: stream 