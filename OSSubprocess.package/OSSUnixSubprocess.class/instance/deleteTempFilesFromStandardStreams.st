cleaning
deleteTempFilesFromStandardStreams
	"Delete all temp files that were associated to standard streams."
	self deleteTempFileOf: stdinStream.
	self deleteTempFileOf: stdoutStream.
	self deleteTempFileOf: stderrStream.
	
	

