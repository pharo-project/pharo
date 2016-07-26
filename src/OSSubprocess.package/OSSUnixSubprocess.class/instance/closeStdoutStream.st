streams
closeStdoutStream
	" Close stdoutStream if needed"
	stdoutStream ifNotNil: [ stdoutStream close ]