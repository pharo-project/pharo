writing
writeOn: stream
	| totalWritten toWrite total  |
	self initializeEncoder.
	totalWritten := 0.
	total := string size.
	[ totalWritten < total ] whileTrue: [ 
		toWrite := ZnUtils streamingBufferSize min: (total - totalWritten).
		encoder next: toWrite putAll: string startingAt: totalWritten + 1 toStream: stream.
		totalWritten := totalWritten + toWrite.
		ZnUtils signalProgress: totalWritten total: total ]