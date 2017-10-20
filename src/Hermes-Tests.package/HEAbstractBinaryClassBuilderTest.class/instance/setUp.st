initialization
setUp
	super setUp.
	fileReference := (FileReference newTempFilePrefix: 'export' suffix: '.exp').
	filename := fileReference fullName.

	writer := HEBinaryReaderWriter new
		stream: (File openForWriteFileNamed:filename);
		yourself.

	reader := HEBinaryReaderWriter new
		stream: (File openForReadFileNamed:filename);
		yourself.
