accessing-files
fileNamed: aFileame readStreamDo: aBlock
	^ aFileame asFileReference readStreamDo: [ :stream |
		stream binary.
		aBlock value: stream ]