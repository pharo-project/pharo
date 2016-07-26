files
makeFileNoneBocking: aFileId
	"Make this stream to be none blocking. In Linux it means 
	calling fcntl() to set the file non-blocking (O_NONBLOCK)."
	self primSQFileSetNonBlocking: aFileId
