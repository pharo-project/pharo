initialize - release
openReadStreamFor: aFileID withName: aReadStreamName
	"Creates a read OSSAttachableFileStream for the read end of the pipe. 
	If the readBlock is false, then make the stream to be non blocking. "
	reader := OSSAttachableFileStream name: aReadStreamName attachTo: aFileID writable: false.
	readBlocking ifFalse: [ reader setNonBlocking ].
	^ reader