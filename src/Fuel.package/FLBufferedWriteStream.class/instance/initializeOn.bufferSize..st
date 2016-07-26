initialize-release
initializeOn: writeStream bufferSize: aSize

	self initialize.
	self initializeStream: writeStream.
	self sizeBuffer: aSize.
	position := 0.