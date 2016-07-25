initialize-release
writeUsingGzipEncodingAndChunkingRepresentation
	message
		setContentEncodingGzip;
		setTransferEncodingChunked.
	representation := self write contents.
	self sizeBuffer: representation size + 1024 