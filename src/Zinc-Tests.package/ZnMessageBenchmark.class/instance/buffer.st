accessing
buffer
	^ buffer ifNil: [  buffer := ByteArray new: ZnUtils streamingBufferSize ]