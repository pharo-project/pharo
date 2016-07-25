streams
closeStdinStream
	" Close stdinStream if needed"
	stdinStream ifNotNil: [ stdinStream close ]