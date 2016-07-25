close and dup2
defineActionsForStdin
	"If needed, automatically create a stream (regular files or pipes) to be used for stdin, and then perform the needed close() and dup2()
	for the mapping in the child"
	self shouldCreateStreamFor: stdinStream ifTrue: [ self redirectStdin ].
	self defineActionsForReadStream: stdinStream dupTo: 0