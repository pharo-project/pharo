cleaning
closeStreams
	"Closes all the streams that have been opened and mapped to standard steams."
	self closeStdinStream.
	self closeStdoutStream.
	self closeStderrStream.
	