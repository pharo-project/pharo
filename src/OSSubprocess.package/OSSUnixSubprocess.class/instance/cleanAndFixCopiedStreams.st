cleaning
cleanAndFixCopiedStreams
	"If the stream is a pipe, we must close the end that we do not use at VM level (parent).
	If we are using regular files, we must reopen them otherwise we won't be able to read what the
	child has written "
	stdinStream ifNotNil: [ stdinStream ossIsPipe ifTrue: [ stdinStream closeReader ] ifFalse: [ stdinStream reopen. ] ].
	stdoutStream ifNotNil: [ stdoutStream ossIsPipe ifTrue: [ stdoutStream closeWriter ] ifFalse: [ stdoutStream reopen. ] ].
	stderrStream ifNotNil: [ stderrStream ossIsPipe ifTrue: [ stderrStream closeWriter ] ifFalse: [ stderrStream reopen. ] ].
	 