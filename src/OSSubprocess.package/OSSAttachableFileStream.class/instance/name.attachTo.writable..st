attaching
name: aSymbolOrString attachTo: aFileID writable: readWriteFlag
	"Attach to an existing file handle, assumed to have been previously
	 opened by the underlying operating system.
	"
	
	name := aSymbolOrString.
	fileID := aFileID.
	readWriteFlag ifTrue: [self readWrite] ifFalse: [self readOnly].
	self ascii.
	self register

