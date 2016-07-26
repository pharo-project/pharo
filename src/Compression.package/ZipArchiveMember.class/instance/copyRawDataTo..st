private-writing
copyRawDataTo: aStream

	[ readDataRemaining > 0 ] whileTrue: [ | data |
		data := self readRawChunk: (4096 min: readDataRemaining).
		aStream nextPutAll: data.
		readDataRemaining := readDataRemaining - data size.
	].
