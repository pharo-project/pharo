accessing-files
fileNamed: aFilename readStreamDo: aBlock
	^ FileStream
		readOnlyFileNamed: aFilename
		do: [ :stream |
			stream binary.
			aBlock value: stream ]