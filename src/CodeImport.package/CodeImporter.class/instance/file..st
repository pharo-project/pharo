accessing
file: aFileStream
	self readStream: aFileStream.
	self requestor: FileCompilerRequestor new