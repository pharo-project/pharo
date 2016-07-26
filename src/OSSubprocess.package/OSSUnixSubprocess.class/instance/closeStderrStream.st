streams
closeStderrStream
	" Close stderrStream if needed"
	stderrStream ifNotNil: [ stderrStream close ]