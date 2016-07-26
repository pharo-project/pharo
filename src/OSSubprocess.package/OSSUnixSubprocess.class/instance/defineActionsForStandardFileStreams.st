close and dup2
defineActionsForStandardFileStreams
	"Define close() and dup2() actions for standard file streams for the child process"
	self defineActionsForStdin.
	self defineActionsForStdout.
	self defineActionsForStderr.
		