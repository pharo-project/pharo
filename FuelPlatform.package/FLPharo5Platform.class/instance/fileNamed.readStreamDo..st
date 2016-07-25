accessing-files
fileNamed: aFilename readStreamDo: aBlock
	^ ((Smalltalk at: #File) named: aFilename) readStreamDo: [ :stream |
		stream binary.
		aBlock value: stream ]