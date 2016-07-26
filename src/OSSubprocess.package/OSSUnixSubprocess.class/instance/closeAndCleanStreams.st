cleaning
closeAndCleanStreams
	"This method closes all the streams that have been opened and mapped to standard steams.
	They could be either pipes or regular temp files. In the case of files, we also remove those temp files"
	self closeStreams.
	self deleteTempFilesFromStandardStreams.