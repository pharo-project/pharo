streaming
streamFrom: inputStream to: outputStream size: totalSize
	| bufferSize buffer leftToRead |
	bufferSize := self streamingBufferSize min: totalSize.
	buffer := (inputStream isBinary ifTrue: [ ByteArray ] ifFalse: [ String ]) new: bufferSize.
	leftToRead := totalSize.
	[ leftToRead > 0 ]
		whileTrue: [ | readCount |
			readCount := bufferSize min: leftToRead.
			inputStream next: readCount into: buffer.
			leftToRead := leftToRead - readCount.
			outputStream next: readCount putAll: buffer startingAt: 1.
			leftToRead > 0 
				ifTrue: [
					self signalProgress: (totalSize - leftToRead) total: totalSize.
					outputStream flush ] ]