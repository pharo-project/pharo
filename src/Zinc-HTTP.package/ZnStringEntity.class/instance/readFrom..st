initialize-release
readFrom: stream
	| buffer totalRead read readStream stringStream total |
	total := self contentLength.
	readStream := total ifNotNil: [ ZnLimitedReadStream on: stream limit: total ] ifNil: [ stream ].
	buffer := String new: (ZnUtils streamingBufferSize min: (total ifNil: [ ZnUtils streamingBufferSize ])).
	stringStream := nil.
	totalRead := 0.
	self initializeEncoder.
	[ readStream atEnd ] whileFalse: [
		[ read := encoder readInto: buffer startingAt: 1 count: buffer size fromStream: readStream ]
			on: ZnByteStringBecameWideString 
			do: [ :notification | 
					buffer := notification wideString.
					stringStream ifNotNil: [ | wideString position |
						position := stringStream position.
						wideString := WideString from: stringStream originalContents.
						stringStream on: wideString; setFrom: position + 1 to: position ].
					notification resume ].
		totalRead := totalRead + read.
		totalRead > ZnMaximumEntitySize value
			ifTrue: [ ZnEntityTooLarge signal ].
		stringStream ifNil: [ 
			readStream atEnd
				ifTrue: [ ^ self string: (buffer copyFrom: 1 to: read) ]
				ifFalse: [ stringStream := (total ifNil: [ buffer species new ] ifNotNil: [ buffer species new: total ]) writeStream ] ].
		stringStream next: read putAll: buffer startingAt: 1.
		ZnUtils signalProgress: totalRead total: total ].
	self string: (stringStream ifNil: [ String new ] ifNotNil: [ stringStream contents ])