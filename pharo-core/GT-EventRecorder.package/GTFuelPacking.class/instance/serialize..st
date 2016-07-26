private
serialize: anObject
	| serializer stream gzstream |
	stream := ByteArray new writeStream binary. 
	gzstream := GZipWriteStream on: stream.
	serializer := FLSerializer newDefault.
	serializer at: #recorderVersion putAdditionalObject: self version.
	serializer stream: gzstream.
	serializer serialize: anObject.
	gzstream close.
	stream close.
	^ stream contents.