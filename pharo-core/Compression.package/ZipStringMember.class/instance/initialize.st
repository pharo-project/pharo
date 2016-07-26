initialization
initialize
	super initialize.
	self contents: ''.
	compressionMethod := desiredCompressionMethod := CompressionStored.
