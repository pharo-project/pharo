private-writing
uncompressDataTo: aStream

	| decoder buffer crcErrorMessage |
	decoder := ZipReadStream on: stream.
	decoder expectedCrc: self crc32.
	buffer := ByteArray new: (32768 min: readDataRemaining).
	crcErrorMessage := nil.

	[[ readDataRemaining > 0 ] whileTrue: [
		| chunkSize |
		chunkSize := 32768 min: readDataRemaining.
		buffer := decoder next: chunkSize into: buffer startingAt: 1.
		aStream next: chunkSize putAll: buffer startingAt: 1.
		readDataRemaining := readDataRemaining - chunkSize.
	]] on: CRCError do: [ :ex | crcErrorMessage := ex messageText. ex resume ].

	crcErrorMessage ifNotNil: [ self isCorrupt: true. CRCError signal: crcErrorMessage ]

