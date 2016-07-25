streaming
bufferedWriteStreamOn: stream
	^ (ZnBufferedWriteStream on: stream)
		sizeBuffer: self streamingBufferSize;
		yourself