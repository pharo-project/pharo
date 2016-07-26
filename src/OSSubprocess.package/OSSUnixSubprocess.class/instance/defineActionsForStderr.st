close and dup2
defineActionsForStderr
	"If needed, automatically create a stream (regular files or pipes) to be used for stderr, and then perform the needed close() and dup2()
	for the mapping in the child"
	self shouldCreateStreamFor: stderrStream ifTrue: [ self redirectStderr ].
	self defineActionsForWriteStream: stderrStream dupTo: 2