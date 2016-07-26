streaming
streamFrom: inputStream to: outputStream
	| buffer totalRead |
	buffer := (inputStream isBinary ifTrue: [ ByteArray ] ifFalse: [ String ]) new: self streamingBufferSize.
	totalRead := 0.
	[ inputStream atEnd ]
		whileFalse: [ | readCount |
			readCount := inputStream readInto: buffer startingAt: 1 count: buffer size.
			readCount > 0
				ifTrue: [
					totalRead := totalRead + readCount.
					outputStream next: readCount putAll: buffer startingAt: 1 ].
			inputStream atEnd
				ifFalse: [
					self signalProgress: totalRead total: nil. 
					outputStream flush ] ]