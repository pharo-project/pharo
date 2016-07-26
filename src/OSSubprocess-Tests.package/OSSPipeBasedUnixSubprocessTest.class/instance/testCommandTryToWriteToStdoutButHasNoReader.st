tests - streams
testCommandTryToWriteToStdoutButHasNoReader
	| command errString  | 
	command := self newCommand
					shellCommand: 'sleep 2; echo 42';
					redirectStdout;
					redirectStderr.
	
	command run.
	command stdoutStream closeReader.				
	command waitForExit.
	"This should be a SIGPIPE because there is no reader opened for the pipe and someone is trying to write to it. " 
	errString := command stderrStream upToEndOfFile.
	self assert: command exitStatusInterpreter exitStatus > 0. "It ended with error".
	"Ideally I would also add this assert, but it seems not all OS write this on stderr. For example,
	OSX does write it, while Linux does not."
	"self assert:( errString includesSubstring: 'Broken pipe')."
	command closeAndCleanStreams.