accessing
file: aFileStream
	self readStream: aFileStream.
	self selectTextConverterForCode.
	self requestor: (FileCompilerRequestor fileStream: aFileStream).