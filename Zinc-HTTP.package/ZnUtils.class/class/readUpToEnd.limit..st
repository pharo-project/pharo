streaming
readUpToEnd: inputStream limit: limit
	| species bufferSize buffer totalRead outputStream |
	bufferSize := limit ifNil: [ self streamingBufferSize ] ifNotNil: [ self streamingBufferSize min: limit ].
	species := inputStream isBinary ifTrue: [ ByteArray ] ifFalse: [ String ].
	buffer := species new: bufferSize.
	totalRead := 0.
	outputStream := nil.
	[ inputStream atEnd ] whileFalse: [ | readCount |
		readCount := inputStream readInto: buffer startingAt: 1 count: bufferSize.
		totalRead := totalRead + readCount.
		(limit notNil and: [ totalRead > limit ])
			ifTrue: [ ZnEntityTooLarge signal ].
		outputStream ifNil: [ 
			inputStream atEnd
				ifTrue: [ ^ buffer copyFrom: 1 to: readCount ] 
				ifFalse: [ outputStream := (species new: bufferSize) writeStream ] ].
		outputStream next: readCount putAll: buffer startingAt: 1.
		self signalProgress: totalRead total: limit ].
	^ outputStream ifNil: [ species new ] ifNotNil: [ outputStream contents ]