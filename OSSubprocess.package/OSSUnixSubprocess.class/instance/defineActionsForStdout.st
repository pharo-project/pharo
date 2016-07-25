close and dup2
defineActionsForStdout
	"If needed, automatically create a stream (regular files or pipes) to be used for stdout, and then perform the needed close() and dup2()
	for the mapping in the child"
	self shouldCreateStreamFor: stdoutStream ifTrue: [ self redirectStdout ].
	self defineActionsForWriteStream: stdoutStream dupTo: 1