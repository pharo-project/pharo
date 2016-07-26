initialize - release
openWriteStreamFor: aFileID withName: aWriteStreamName
	"Creates a write OSSAttachableFileStream for the write end of the pipe.  "
	writer := OSSAttachableFileStream name: aWriteStreamName attachTo: aFileID writable: true.
	^ writer