private-writing
copyRawDataTo: aStream from: start to: finish

	readDataRemaining := readDataRemaining min: finish - start + 1.

	self readRawChunk: start - 1.

	[ readDataRemaining > 0 ] whileTrue: [ | data |
		data := self readRawChunk: (32768 min: readDataRemaining).
		aStream nextPutAll: data.
		readDataRemaining := readDataRemaining - data size.
	].
