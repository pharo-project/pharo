accessing-files
fileNamed: aFilename writeStreamDo: aBlock
	^ ((Smalltalk at: #File) named: aFilename) writeStreamDo: [ :stream |
		stream binary.
		aBlock value: stream ]