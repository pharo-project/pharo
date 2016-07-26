accessing-files
fileNamed: aFilename writeStreamDo: aBlock
	^ FileStream
		forceNewFileNamed: aFilename
		do: [ :stream |
			stream binary.
			aBlock value: stream ]